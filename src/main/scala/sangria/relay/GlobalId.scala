package sangria.relay

import sangria.relay.util.Base64

case class GlobalId(typeName: String, id: String) {
  def asString = GlobalId.toGlobalId(this)
}

object GlobalId {
  def toGlobalId(globalId: GlobalId): String = toGlobalId(globalId.typeName, globalId.id)

  /**
   * Takes a type name and an ID specific to that type name, and returns a
   * "global ID" that is unique among all types.
   */
  def toGlobalId(typeName: String, id: String): String = Base64.encode(s"$typeName:$id")

  /**
   * Takes the "global ID" created by toGlobalID, and retuns the type name and ID
   * used to create it.
   */
  def fromGlobalId(globalId: String) = {
    val parts = Base64.decode(globalId).split(":")

    if (parts.size != 2)
      None
    else
      Some(GlobalId(parts(0), parts(1)))
  }

  def unapply(globalId: String) = fromGlobalId(globalId)
}
