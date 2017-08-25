package sangria.relay.util

import java.nio.charset.StandardCharsets

import scala.util.Try

object Base64 {
  // Java 7 solution. Use java.util.Base64 in Java 8
  import javax.xml.bind.DatatypeConverter

  def encode(bytes: Array[Byte]): String =
    DatatypeConverter.printBase64Binary(bytes)

  def encode(string: String): String =
    DatatypeConverter.printBase64Binary(string.getBytes(StandardCharsets.UTF_8))

  def decode(base64String: String): Option[String] =
    Try(DatatypeConverter.parseBase64Binary(base64String)).map(new String(_, StandardCharsets.UTF_8)).toOption
}
