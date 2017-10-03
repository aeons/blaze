package org.http4s.blaze.http.endtoend.scaffolds

import java.util.concurrent.TimeUnit

import org.asynchttpclient.{Dsl, Request, RequestBuilder, Response}
import org.http4s.blaze.http.{HttpRequest, HttpResponsePrelude}

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** Basic scaffold for running http requests against a server
 *
  * @param timeout timeout for request. Must be finite and greater than 0
  */
final class AsyncHttpClientScaffold(timeout: Duration) extends ClientScaffold(1, 1) {
  require(timeout.isFinite && timeout.length > 0)

  private val client = Dsl.asyncHttpClient(
    Dsl.config()
      .setMaxConnections(500)
      .setMaxConnectionsPerHost(200)
      .setPooledConnectionIdleTimeout(100)
      .setConnectionTtl(500)
  )

  // Not afraid to block: this is for testing.
  override def runRequest(request: HttpRequest): (HttpResponsePrelude, Array[Byte]) = {
    val asyncHttpRequest = requestToAsyncHttpRequest(request)
    val response = runAsyncHttpRequest(asyncHttpRequest)
    asyncHttpResponseToResponse(response)
  }

  private[this] def requestToAsyncHttpRequest(request: HttpRequest): Request = {
    val body = Await.result(request.body.accumulate(), timeout)

    val requestBuilder = new RequestBuilder()
    requestBuilder.setUrl(request.url)
    requestBuilder.setMethod(request.method)
    request.headers.foreach { case (k, v) =>
      requestBuilder.setHeader(k, v)
    }

    requestBuilder.setBody(body)
    requestBuilder.build()
  }

  private[this] def asyncHttpResponseToResponse(resp: Response): (HttpResponsePrelude, Array[Byte]) = {
    val statusCode = resp.getStatusCode
    val status = resp.getStatusText
    val hs = resp.getHeaders.entries().asScala.map(e => e.getKey -> e.getValue)
    val response = HttpResponsePrelude(statusCode, status, hs)

    response -> resp.getResponseBodyAsBytes
  }

  // Not afraid to block: this is for testing.
  private[this] def runAsyncHttpRequest(request: Request): Response = {
    client.prepareRequest(request)
      .execute()
      .toCompletableFuture
      .get(timeout.toMillis, TimeUnit.MILLISECONDS)
  }

  def close(): Unit = {
    client.close()
  }
}