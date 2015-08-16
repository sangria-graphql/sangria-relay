package sangria.relay

import language.higherKinds

import sangria.relay.util.Base64
import sangria.schema._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.Try

import scala.annotation.implicitNotFound

trait Connection[T] {
  def pageInfo: PageInfo
  def edges: Seq[Edge[T]]
}

object Connection {
  object Args {
    val Before = Argument("before", OptionInputType(StringType))
    val After = Argument("after", OptionInputType(StringType))
    val First = Argument("first", OptionInputType(IntType))
    val Last = Argument("last", OptionInputType(IntType))

    val All = Before :: After :: First :: Last :: Nil
  }

  def definition[Ctx, Conn[_], Val](
    name: String,
    nodeType: ObjectType[Ctx, Val],
    edgeFields: => List[Field[Ctx, Edge[Val]]] = Nil,
    connectionFields: => List[Field[Ctx, Conn[Val]]] = Nil
  )(implicit connEv: ConnectionLike[Conn, Val], classEv: ClassTag[Conn[Val]]) = {
    val edgeType = ObjectType[Ctx, Edge[Val]](name + "Edge", "An edge in a connection.",
      () => {
        List[Field[Ctx, Edge[Val]]](
          Field("node", nodeType, Some("The item at the end of the edge."), resolve = _.value.node),
          Field("cursor", StringType, Some("A cursor for use in pagination."), resolve = _.value.cursor)
        ) ++ edgeFields
      })

    val connectionType = ObjectType[Ctx, Conn[Val]](name + "Connection", "A connection to a list of items.",
      () => {
        List[Field[Ctx, Conn[Val]]](
          Field("pageInfo", PageInfoType, Some("Information to aid in pagination."), resolve = ctx => connEv.pageInfo(ctx.value)),
          Field("edges", OptionType(ListType(OptionType(edgeType))), Some("Information to aid in pagination."), resolve = ctx => connEv.edges(ctx.value) map (Some(_)))
        ) ++ connectionFields
      })

    ConnectionDefinition(edgeType, connectionType)
  }

  /**
   * The common page info type used by all connections.
   */
  val PageInfoType = ObjectType("PageInfo", "Information about pagination in a connection.",
    fields[Unit, PageInfo](
      Field("hasNextPage", BooleanType, Some("When paginating forwards, are there more items?"),
        resolve = _.value.hasNextPage),
      Field("hasPreviousPage", BooleanType, Some("When paginating backwards, are there more items?"),
        resolve = _.value.hasPreviousPage),
      Field("startCursor", OptionType(StringType), Some("When paginating backwards, the cursor to continue."),
        resolve = _.value.startCursor),
      Field("endCursor", OptionType(StringType), Some("When paginating forwards, the cursor to continue."),
        resolve = _.value.endCursor)
    ))

  val CursorPrefix = "arrayconnection:"

  def empty[T] = DefaultConnection(PageInfo.empty, Vector.empty[Edge[T]])

  def connectionFromFutureSeq[T](coll: Future[Seq[T]], args: ConnectionArgs)(implicit ec: ExecutionContext) =
    coll map (connectionFromSeq(_, args))

  def connectionFromSeq[T](coll: Seq[T], args: ConnectionArgs) = {
    val edges = coll.zipWithIndex map {case (elem, idx) => Edge(elem, offsetToCursor(idx))}

    val begin = math.max(getOffset(args.after, -1), -1) + 1
    val end = math.min(getOffset(args.before, edges.size + 1), edges.size + 1)

    val slicedEdges = edges.slice(begin, end)

    if (slicedEdges.isEmpty) Connection.empty[T]
    else {
      val firstPresliceCursor = slicedEdges.head.cursor
      val lastPresliceCursor = slicedEdges.last.cursor

      val withFirst = args.first.fold(slicedEdges)(slicedEdges.take)
      val withLast = args.last.fold(withFirst)(withFirst.takeRight)

      if (withLast.isEmpty) Connection.empty[T]
      else {
        val firstEdge = withLast.head
        val lastEdge = withLast.last

        DefaultConnection(
          PageInfo(
            startCursor = Some(firstEdge.cursor),
            endCursor = Some(lastEdge.cursor),
            hasPreviousPage = firstEdge.cursor != firstPresliceCursor,
            hasNextPage = lastEdge.cursor != lastPresliceCursor),
          withLast)
      }
    }
  }

  def cursorForObjectInConnection[T](coll: Seq[T], obj: T) = {
    val idx = coll.indexOf(obj)

    if (idx  >= 0) Some(offsetToCursor(idx)) else None
  }

  private def getOffset(cursor: Option[String], defaultOffset: Int) =
    cursor flatMap cursorToOffset getOrElse defaultOffset

  private def offsetToCursor(offset: Int) = Base64.encode(CursorPrefix + offset)

  private def cursorToOffset(cursor: String) =
    GlobalId.fromGlobalId(cursor).flatMap(id => Try(id.id.toInt).toOption)


}

case class ConnectionDefinition[Ctx, Conn, Val](edgeType: ObjectType[Ctx, Edge[Val]], connectionType: ObjectType[Ctx, Conn])

case class DefaultConnection[T](pageInfo: PageInfo, edges: Seq[Edge[T]]) extends Connection[T]

trait Edge[T] {
  def node: T
  def cursor: String
}

object Edge {
  def apply[T](node: T, cursor: String) = DefaultEdge(node, cursor)
}

case class DefaultEdge[T](node: T, cursor: String) extends Edge[T]

case class PageInfo(
  hasNextPage: Boolean = false,
  hasPreviousPage: Boolean = false,
  startCursor: Option[String] = None,
  endCursor: Option[String] = None)

object PageInfo {
  def empty = PageInfo()
}

@implicitNotFound("Type ${T} can't be used as a Connection. Please consider defining implicit instance of sangria.relay.ConnectionLike for type ${T} or extending sangria.relay.Connection trait.")
trait ConnectionLike[T[_], E] {
  def pageInfo(conn: T[E]): PageInfo
  def edges(conn: T[E]): Seq[Edge[E]]
}

object ConnectionLike {
  private object ConnectionIsConnectionLike extends ConnectionLike[Connection, Any] {
    override def pageInfo(conn: Connection[Any]) = conn.pageInfo
    override def edges(conn: Connection[Any]) = conn.edges
  }

  implicit def connectionIsConnectionLike[E, T[_]]: ConnectionLike[T, E] =
    ConnectionIsConnectionLike.asInstanceOf[ConnectionLike[T, E]]
}

case class ConnectionArgs(before: Option[String] = None, after: Option[String] = None, first: Option[Int] = None, last: Option[Int] = None)

object ConnectionArgs {
  def apply(args: WithArguments): ConnectionArgs =
    ConnectionArgs(
      args argOpt Connection.Args.Before,
      args argOpt Connection.Args.After,
      args argOpt Connection.Args.First,
      args argOpt Connection.Args.Last)

  val empty = ConnectionArgs()
}