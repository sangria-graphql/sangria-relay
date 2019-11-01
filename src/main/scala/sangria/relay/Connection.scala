package sangria.relay

import sangria.execution.UserFacingError
import sangria.relay.util.Base64
import sangria.schema._

import scala.annotation.implicitNotFound
import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Try

trait Connection[+T] {
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

  def isValidNodeType[Val](nodeType: OutputType[Val]): Boolean = nodeType match {
    case _: ScalarType[_] | _: EnumType[_] | _: CompositeType[_] => true
    case OptionType(ofType) => isValidNodeType(ofType)
    case _ => false
  }

  def definition[Ctx, Conn[_], Val](
    name: String,
    nodeType: OutputType[Val],
    edgeFields: => List[Field[Ctx, Edge[Val]]] = Nil,
    connectionFields: => List[Field[Ctx, Conn[Val]]] = Nil
  )(implicit connEv: ConnectionLike[Conn, PageInfo, Val, Edge[Val]], classEv: ClassTag[Conn[Val]]) = {
    definitionWithEdge[Ctx, DefaultPageInfo, Conn, Val, Edge[Val]](name, nodeType, edgeFields, connectionFields)
  }

  def definitionWithEdge[Ctx, P <: PageInfo, Conn[_], Val, E <: Edge[Val]](
    name: String,
    nodeType: OutputType[Val],
    edgeFields: => List[Field[Ctx, E]] = Nil,
    connectionFields: => List[Field[Ctx, Conn[Val]]] = Nil,
    pageInfoType: OutputType[P] = DefaultPageInfo.pageInfoType
  )(implicit connEv: ConnectionLike[Conn, P, Val, E], classEv: ClassTag[Conn[Val]], classE: ClassTag[E], classP: ClassTag[P]) = {
    if (!isValidNodeType(nodeType))
      throw new IllegalArgumentException("Node type is invalid. It must be either a Scalar, Enum, Object, Interface, Union, " +
          "or a Non‐Null wrapper around one of those types. Notably, this field cannot return a list.")

    val edgeType = ObjectType[Ctx, E](name + "Edge", "An edge in a connection.",
      () => {
        List[Field[Ctx, E]](
          Field("node", nodeType, Some("The item at the end of the edge."), resolve = _.value.node),
          Field("cursor", StringType, Some("A cursor for use in pagination."), resolve = _.value.cursor)
        ) ++ edgeFields
      })

    val connectionType = ObjectType[Ctx, Conn[Val]](name + "Connection", "A connection to a list of items.",
      () => {
        List[Field[Ctx, Conn[Val]]](
          Field("pageInfo", pageInfoType, Some("Information to aid in pagination."), resolve = ctx => connEv.pageInfo(ctx.value)),
          Field("edges", OptionType(ListType(OptionType(edgeType))), Some("A list of edges."),
            resolve = ctx => connEv.edges(ctx.value) map (Some(_)))
        ) ++ connectionFields
      })

    ConnectionDefinition[Ctx, Conn[Val], Val, E](edgeType, connectionType)
  }

  val CursorPrefix = "arrayconnection:"

  def empty[T] = DefaultConnection(PageInfo.empty, Vector.empty[Edge[T]])

  def connectionFromFutureSeq[T](seq: Future[Seq[T]], args: ConnectionArgs)(implicit ec: ExecutionContext): Future[Connection[T]] =
    seq map (connectionFromSeq(_, args))

  def connectionFromSeq[T](seq: Seq[T], args: ConnectionArgs): Connection[T] =
    connectionFromSeq(seq, args, SliceInfo(0, seq.size))

  def connectionFromFutureSeq[T](seq: Future[Seq[T]], args: ConnectionArgs, sliceInfo: SliceInfo)(implicit ec: ExecutionContext): Future[Connection[T]] =
    seq map (connectionFromSeq(_, args, sliceInfo))

  def connectionFromSeq[T](seqSlice: Seq[T], args: ConnectionArgs, sliceInfo: SliceInfo): Connection[T] = {
    import args._
    import sliceInfo._

    first.foreach(f => if (f < 0) throw ConnectionArgumentValidationError("Argument 'first' must be a non-negative integer"))
    last.foreach(l => if (l < 0) throw ConnectionArgumentValidationError("Argument 'last' must be a non-negative integer"))

    val sliceEnd = sliceStart + seqSlice.size
    val beforeOffset = getOffset(before, size)
    val afterOffset = getOffset(after, -1)

    val startOffset = math.max(math.max(sliceStart - 1, afterOffset), -1) + 1
    val endOffset = math.min(math.min(sliceEnd, beforeOffset), size)

    val actualEndOffset = first.fold(endOffset)(f => math.min(endOffset, startOffset + f))
    val actualStartOffset = last.fold(startOffset)(l => math.max(startOffset, actualEndOffset - l))

    // If supplied slice is too large, trim it down before mapping over it.
    val slice = seqSlice.slice(math.max(actualStartOffset - sliceStart, 0), seqSlice.size - (sliceEnd - actualEndOffset))

    val edges = slice.zipWithIndex.map {
      case (value, index) => Edge(value, offsetToCursor(actualStartOffset + index))
    }

    val firstEdge = edges.headOption
    val lastEdge = edges.lastOption
    val lowerBound = after.fold(0)(_ => afterOffset + 1)
    val upperBound = before.fold(size)(_ => beforeOffset)

    DefaultConnection(
      PageInfo(
        startCursor = firstEdge map (_.cursor),
        endCursor = lastEdge map (_.cursor),
        hasPreviousPage = last.fold(false)(_ => actualStartOffset > lowerBound),
        hasNextPage = first.fold(false)(_ => actualEndOffset < upperBound)),
      edges
    )
  }

  def cursorForObjectInConnection[T, E](coll: Seq[T], obj: E): Option[String] = {
    val idx = coll.indexOf(obj)

    if (idx  >= 0) Some(offsetToCursor(idx)) else None
  }

  private def getOffset(cursor: Option[String], defaultOffset: Int): Int =
    cursor flatMap cursorToOffset getOrElse defaultOffset

  def offsetToCursor(offset: Int): String = Base64.encode(CursorPrefix + offset)

  def cursorToOffset(cursor: String): Option[Int] =
    GlobalId.fromGlobalId(cursor).flatMap(id => Try(id.id.toInt).toOption)
}

case class SliceInfo(sliceStart: Int, size: Int)

case class ConnectionDefinition[Ctx, Conn, Val, E <: Edge[Val]](edgeType: ObjectType[Ctx, E], connectionType: ObjectType[Ctx, Conn])

case class DefaultConnection[T](pageInfo: PageInfo, edges: Seq[Edge[T]]) extends Connection[T]

trait Edge[+T] {
  def node: T
  def cursor: String
}

object Edge {
  def apply[T](node: T, cursor: String) = DefaultEdge(node, cursor)
}

case class DefaultEdge[T](node: T, cursor: String) extends Edge[T]

trait PageInfo {
  def hasNextPage: Boolean
  def hasPreviousPage: Boolean
  def startCursor: Option[String]
  def endCursor: Option[String]
}

case class DefaultPageInfo(
  hasNextPage: Boolean = false,
  hasPreviousPage: Boolean = false,
  startCursor: Option[String] = None,
  endCursor: Option[String] = None) extends PageInfo

object DefaultPageInfo {
  def pageInfoType[Ctx, P <: PageInfo : ClassTag]: ObjectType[Ctx, P] =
    ObjectType("PageInfo", "Information about pagination in a connection.",
      () => {
        List[Field[Ctx, P]](
          Field("hasNextPage", BooleanType, Some("When paginating forwards, are there more items?"),
            resolve = _.value.hasNextPage),
          Field("hasPreviousPage", BooleanType, Some("When paginating backwards, are there more items?"),
            resolve = _.value.hasPreviousPage),
          Field("startCursor", OptionType(StringType), Some("When paginating backwards, the cursor to continue."),
            resolve = _.value.startCursor),
          Field("endCursor", OptionType(StringType), Some("When paginating forwards, the cursor to continue."),
            resolve = _.value.endCursor)
        )
      })
}

object PageInfo {
  def empty: DefaultPageInfo = DefaultPageInfo()

  def apply(
    hasNextPage: Boolean = false,
    hasPreviousPage: Boolean = false,
    startCursor: Option[String] = None,
    endCursor: Option[String] = None
  ): PageInfo = DefaultPageInfo(hasNextPage, hasPreviousPage, startCursor, endCursor)

}

@implicitNotFound("Type ${T} can't be used as a Connection. Please consider defining implicit instance of sangria.relay.ConnectionLike for type ${T} or extending sangria.relay.Connection trait.")
trait ConnectionLike[T[_], P <: PageInfo, Val, E <: Edge[Val]] {
  def pageInfo(conn: T[Val]): P
  def edges(conn: T[Val]): Seq[E]
}

object ConnectionLike {
  private object ConnectionIsConnectionLike extends ConnectionLike[Connection, PageInfo, Any, Edge[Any]] {
    override def pageInfo(conn: Connection[Any]) = conn.pageInfo
    override def edges(conn: Connection[Any]) = conn.edges
  }

  implicit def connectionIsConnectionLike[E <: Edge[Val], P <: PageInfo, Val, T[_]]: ConnectionLike[T, P, Val, E] =
    ConnectionIsConnectionLike.asInstanceOf[ConnectionLike[T, P, Val, E]]
}

case class ConnectionArgs(before: Option[String] = None, after: Option[String] = None, first: Option[Int] = None, last: Option[Int] = None)

object ConnectionArgs {
  def apply(args: WithArguments): ConnectionArgs =
    ConnectionArgs(
      args arg Connection.Args.Before,
      args arg Connection.Args.After,
      args arg Connection.Args.First,
      args arg Connection.Args.Last)

  val empty = ConnectionArgs()
}

case class ConnectionArgumentValidationError(message: String) extends Exception with UserFacingError
