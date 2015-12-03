package sangria.relay.util

trait ResultHelper {
  implicit class Helper(any: Any) {
    def getProp(name: String) = any.asInstanceOf[Map[String, Any]](name)
    def asList = any.asInstanceOf[Vector[Any]]
    def asString = any.asInstanceOf[String]
    def asAnyRef = any.asInstanceOf[AnyRef]
  }
}
