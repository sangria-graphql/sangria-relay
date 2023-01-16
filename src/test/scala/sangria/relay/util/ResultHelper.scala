package sangria.relay.util

trait ResultHelper {
  implicit class Helper(any: Any) {
    def asMap: Map[String, Any] = any.asInstanceOf[Map[String, Any]]
    def getProp(name: String) = asMap(name)
    def asList = any.asInstanceOf[Vector[Any]]
    def asString = any.asInstanceOf[String]
    def asAnyRef = any.asInstanceOf[AnyRef]
  }
}
