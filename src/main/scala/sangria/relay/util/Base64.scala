package sangria.relay.util

import java.nio.charset.StandardCharsets

import scala.util.Try

object Base64 {
  private def toString(bytes: Array[Byte]) = new String(bytes, StandardCharsets.UTF_8)
  
  def encode(bytes: Array[Byte]): String = toString(java.util.Base64.getEncoder.encode(bytes))

  def encode(string: String): String = encode(string.getBytes(StandardCharsets.UTF_8))

  def decode(base64String: String): Option[String] =
    Try(java.util.Base64.getDecoder.decode(base64String)).map(toString).toOption
}
