package org.http4s.blaze.http.http2

import java.nio.ByteBuffer

import org.http4s.blaze.http.http2.Priority.Dependant
import org.http4s.blaze.http.http2.bits.Flags
import org.specs2.mutable.Specification

class Http2FrameDecoderSpec extends Specification {

  def buffer(data: Byte*): ByteBuffer =
    ByteBuffer.wrap(data.toArray)

  //  +-----------------------------------------------+
  //  |                 Length (24)                   |
  //  +---------------+---------------+---------------+
  //  |   Type (8)    |   Flags (8)   |
  //  +-+-------------+---------------+-------------------------------+
  //  |R|                 Stream Identifier (31)                      |
  //  +=+=============================================================+
  //  |                   Frame Payload (0...)                      ...
  //  +---------------------------------------------------------------+

  "DATA" >> {
    class DataListener extends MockFrameListener(false) {
      var streamId: Option[Int] = None
      var endStream: Option[Boolean] = None
      var data: ByteBuffer = null
      var flowSize: Option[Int] = None
      override def onDataFrame(streamId: Int, endStream: Boolean, data: ByteBuffer, flowSize: Int): Http2Result = {
        this.streamId = Some(streamId)
        this.endStream = Some(endStream)
        this.data = data
        this.flowSize = Some(flowSize)
        Continue
      }
    }

    "Decode basic frame" >> {
      // UNKONWN Frame:
      // Length: 8, Type: 0x16, Flags: 0, R: 0, StreamID: 0
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x00, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new DataListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)
      dec.decodeBuffer(testData) must_== Continue

      listener.streamId = Some(1)
      listener.endStream must_== Some(false)
      listener.data must_== ByteBuffer.wrap(new Array[Byte](8))
      listener.flowSize must_== Some(8)
    }

    "Decode basic frame without data" >> {
      // UNKONWN Frame:
      // Length: 8, Type: 0x16, Flags: 0, R: 0, StreamID: 0
      val testData = buffer(
        0x00, 0x00, 0x00, // length
        0x00, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01 // streamId
        ) // no data
      val listener = new DataListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)
      dec.decodeBuffer(testData) must_== Continue

      listener.streamId = Some(1)
      listener.endStream must_== Some(false)
      listener.data must_== ByteBuffer.wrap(new Array[Byte](0))
      listener.flowSize must_== Some(0)
    }

    "basic frame with end-stream" >> {
      // UNKONWN Frame:
      // Length: 8, Type: 0x16, Flags: 0, R: 0, StreamID: 0
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x00, // type
        Flags.END_STREAM, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new DataListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)
      dec.decodeBuffer(testData) must_== Continue

      listener.endStream must_== Some(true)
    }

    "basic frame with padding of 0 length" >> {
      // UNKONWN Frame:
      // Length: 8, Type: 0x16, Flags: 0, R: 0, StreamID: 0
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x00, // type
        Flags.PADDED, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, // padding length
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new DataListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)
      dec.decodeBuffer(testData) must_== Continue

      listener.streamId = Some(1)
      listener.endStream must_== Some(false)
      listener.data must_== ByteBuffer.wrap(new Array[Byte](7))
      listener.flowSize must_== Some(8)
    }

    "basic frame with padding of length equal to the remaining body length" >> {
      // UNKONWN Frame:
      // Length: 8, Type: 0x16, Flags: 0, R: 0, StreamID: 0
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x00, // type
        Flags.PADDED, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x07, // padding length
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new DataListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)
      dec.decodeBuffer(testData) must_== Continue

      listener.streamId = Some(1)
      listener.endStream must_== Some(false)
      listener.data must_== buffer()
      listener.flowSize must_== Some(8)
    }

    "basic frame with padding of length equal to the body" >> {
      // UNKONWN Frame:
      // Length: 8, Type: 0x16, Flags: 0, R: 0, StreamID: 0
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x00, // type
        Flags.PADDED, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x08, // padding length
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new DataListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)
      dec.decodeBuffer(testData) must beLike {
        case Error(_: Http2SessionException) => ok
      }
    }

    "not allow DATA on stream 0" >> {
      // DATA frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x00, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x00, // streamId
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new DataListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)
      dec.decodeBuffer(testData) must beLike {
        case Error(_: Http2SessionException) => ok
      }
    }
  }

  //  +---------------+
  //  |Pad Length? (8)|
  //  +-+-------------+-----------------------------------------------+
  //  |E|                 Stream Dependency? (31)                     |
  //  +-+-------------+-----------------------------------------------+
  //  |  Weight? (8)  |
  //  +-+-------------+-----------------------------------------------+
  //  |                   Header Block Fragment (*)                 ...
  //  +---------------------------------------------------------------+
  //  |                           Padding (*)                       ...
  //  +---------------------------------------------------------------+

  "HEADERS" >> {
    class HeadersListener extends MockFrameListener(false) {
      // For handling unknown stream frames
      var streamId: Option[Int] = None
      var priority: Option[Priority] = None
      var endHeaders: Option[Boolean] = None
      var endStream: Option[Boolean] = None
      var buffer: ByteBuffer = null

      override def onHeadersFrame(streamId: Int,
                                  priority: Priority,
                                  end_headers: Boolean,
                                  end_stream: Boolean,
                                  buffer: ByteBuffer): Http2Result = {
        this.streamId = Some(streamId)
        this.priority = Some(priority)
        this.endHeaders = Some(end_headers)
        this.endStream = Some(end_stream)
        this.buffer = buffer
        Continue
      }
    }

    "basic HEADERS" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
      listener.streamId must beSome(1)
      listener.priority must beSome(Priority.NoPriority)
      listener.endHeaders must beSome(false)
      listener.endStream must beSome(false)
      listener.buffer must_== ByteBuffer.wrap(new Array(8))
    }

    "basic HEADERS with exclusive priority" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        Flags.PRIORITY, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        (1 << 7).toByte, 0x00, 0x00, 0x02, // stream dependency, exclusive
        0xff.toByte, // weight
        0x00, 0x00, 0x00)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
      listener.streamId must beSome(1)
      listener.priority must beSome(Priority.Dependant(2, true, 256))
      listener.endHeaders must beSome(false)
      listener.endStream must beSome(false)
      listener.buffer must_== ByteBuffer.wrap(new Array(3))
    }

    "basic HEADERS with pad length 0" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        Flags.PADDED, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
      listener.streamId must beSome(1)
      listener.priority must beSome(Priority.NoPriority)
      listener.endHeaders must beSome(false)
      listener.endStream must beSome(false)
      listener.buffer must_== ByteBuffer.wrap(new Array(7))
    }

    "basic HEADERS with padding" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        (Flags.PADDED | Flags.PRIORITY).toByte, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x01, // padding of 1
        0x00, 0x00, 0x00, 0x02, // dependent stream 2, non-exclusive
        0x02, // weight 3
        0x00, 0x01)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
      listener.streamId must beSome(1)
      listener.priority must beSome(Priority.Dependant(2, false, 3))
      listener.endHeaders must beSome(false)
      listener.endStream must beSome(false)
      listener.buffer must_== buffer(0x00)
    }

    "basic HEADERS with padding of remaining size" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        Flags.PADDED, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x07, // padding of 7
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
      listener.streamId must beSome(1)
      listener.priority must beSome(Priority.NoPriority)
      listener.endHeaders must beSome(false)
      listener.endStream must beSome(false)
      listener.buffer must_== buffer()
    }

    "not allow HEADERS on stream 0" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x00, // streamId
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must beLike {
        case Error(_: Http2SessionException) => ok
      }
    }

    "HEADERS with priority on itself" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        Flags.PRIORITY, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x01, // dependent stream 1, non-exclusive
        0x02, // weight 3
        0x00, 0x00, 0x01)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must beLike {
        case Error(_: Http2SessionException) => ok
      }
    }

    "HEADERS with dependency on stream 0" >> {
      // HEADERS frame:
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x01, // type
        Flags.PRIORITY, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x00, // dependent stream 0, non-exclusive
        0x02, // weight 3
        0x00, 0x00, 0x01)
      val listener = new HeadersListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
    }
  }

  "PRIORITY" >> {

    class PriorityListener extends MockFrameListener(false) {
      var streamId: Option[Int] = None
      var priority: Option[Dependant] = None
      override def onPriorityFrame(streamId: Int, priority: Dependant): Http2Result = {
        this.streamId = Some(streamId)
        this.priority = Some(priority)
        Continue
      }
    }

    //  +-+-------------------------------------------------------------+
    //  |E|                  Stream Dependency (31)                     |
    //  +-+-------------+-----------------------------------------------+
    //  |   Weight (8)  |
    //  +-+-------------+

    "simple PRIORITY frame" >> {
      // PRIORITY frame:
      val testData = buffer(
        0x00, 0x00, 0x05, // length
        0x02, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x02,
        0x00)
      val listener = new PriorityListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
      listener.streamId must beSome(1)
      listener.priority must beSome(Priority.Dependant(2, false, 1))
    }

    "simple PRIORITY frame with exclusive" >> {
      // PRIORITY frame:
      val testData = buffer(
        0x00, 0x00, 0x05, // length
        0x02, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        (1 << 7).toByte, 0x00, 0x00, 0x02, // stream dependency 2
        0x00)
      val listener = new PriorityListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must_== Continue
      listener.streamId must beSome(1)
      listener.priority must beSome(Priority.Dependant(2, true, 1))
    }

    "frame with dependent stream being identity stream" >> {
      // PRIORITY frame:
      val testData = buffer(
        0x00, 0x00, 0x05, // length
        0x02, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x01, // stream dependency
        0x00) // weight
      val listener = new PriorityListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must beLike {
        case Error(_: Http2SessionException) => ok
      }
    }

    "frame with too large of body" >> {
      // PRIORITY frame:
      val testData = buffer(
        0x00, 0x00, 0x06, // length
        0x02, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x02, // stream dependency
        0x00,  // weight
        0x11) // extra byte
      val listener = new PriorityListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must beLike {
        case Error(_: Http2StreamException) => ok
      }
    }

    "frame with too small of body" >> {
      // PRIORITY frame:
      val testData = buffer(
        0x00, 0x00, 0x04, // length
        0x02, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x01, // streamId
        0x00, 0x00, 0x00, 0x02 // stream dependency
        ) // missing weight
      val listener = new PriorityListener
      val dec = new Http2FrameDecoder(Http2Settings.default, listener)

      dec.decodeBuffer(testData) must beLike {
        case Error(_: Http2StreamException) => ok
      }
    }
  }

//  case FrameTypes.RST_STREAM    => decodeRstStreamFrame(buffer, streamId)
//  case FrameTypes.SETTINGS      => decodeSettingsFrame(buffer, streamId, flags)
//  case FrameTypes.PUSH_PROMISE  => decodePushPromiseFrame(buffer, streamId, flags)
//  case FrameTypes.PING          => decodePingFrame(buffer, streamId, flags)
//  case FrameTypes.GOAWAY        => decodeGoAwayFrame(buffer, streamId)
//  case FrameTypes.WINDOW_UPDATE => decodeWindowUpdateFrame(buffer, streamId)
//  case FrameTypes.CONTINUATION  => decodeContinuationFrame(buffer, streamId, flags)

  "unknown frame types" >> {
    "pass the data to the extension frame method" in {

      // UNKONWN Frame:
      // Length: 8, Type: 0x16, Flags: 0, R: 0, StreamID: 0
      val testData = buffer(
        0x00, 0x00, 0x08, // length
        0x16, // type
        0x00, // flags
        0x00, 0x00, 0x00, 0x00, // streamId
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)

      val listener = new MockFrameListener(false)
      val dec = new Http2FrameDecoder(Http2Settings.default, listener) {
        var data: ByteBuffer = null
        var code: Option[Byte] = None
        var flags: Option[Byte] = None
        var streamId: Option[Int] = None

        override def onExtensionFrame(_code: Byte, _streamId: Int, _flags: Byte, buffer: ByteBuffer): Http2Result = {
          data = buffer
          code = Some(_code)
          streamId = Some(_streamId)
          flags = Some(_flags)
          Continue
        }
      }

      dec.decodeBuffer(testData) must_== Continue
      dec.data must_== ByteBuffer.wrap(new Array[Byte](8))
      dec.code must_== Some(0x16)
      dec.flags must_== Some(0x00)
      dec.streamId must_== Some(0)
    }
  }
}
