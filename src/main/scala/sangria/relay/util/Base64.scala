package sangria.relay.util

import java.nio.charset.Charset

object Base64 {
  // Java 7 solution. Use java.util.Base64 in Java 8
  import javax.xml.bind.DatatypeConverter

  val `UTF-8` = Charset.forName("UTF-8")

  def encode(bytes: Array[Byte]): String =
    DatatypeConverter.printBase64Binary(bytes)

  def encode(string: String): String =
    DatatypeConverter.printBase64Binary(string.getBytes(`UTF-8`))

  def decode(base64String: String): String =
    new String(DatatypeConverter.parseBase64Binary(base64String), `UTF-8`)
}
