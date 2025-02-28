package sangria.relay

import sangria.execution.UserFacingError
import sangria.relay.util.Base64
import sangria.schema._

import scala.annotation.{implicitNotFound, tailrec}
import scala.concurrent.{ExecutionContext, Future}
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

  @tailrec
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
  )(implicit
      connEv: ConnectionLike[Conn, PageInfo, Val, Edge[Val]],
      classEv: ClassTag[Conn[Val]]): ConnectionDefinition[Ctx, Conn[Val], Val, Edge[Val]] =
    definitionWithEdge[Ctx, DefaultPageInfo, Conn, Val, Edge[Val]](
      name,
      nodeType,
      edgeFields,
      connectionFields)

  def definitionWithEdge[Ctx, P <: PageInfo, Conn[_], Val, E <: Edge[Val]](
      name: String,
      nodeType: OutputType[Val],
      edgeFields: => List[Field[Ctx, E]] = Nil,
      connectionFields: => List[Field[Ctx, Conn[Val]]] = Nil,
      pageInfoType: Option[OutputType[P]] = None
  )(implicit
      connEv: ConnectionLike[Conn, P, Val, E],
      classEv: ClassTag[Conn[Val]],
      classE: ClassTag[E],
      classP: ClassTag[P]) = {
    if (!isValidNodeType(nodeType))
      throw new IllegalArgumentException(
        "Node type is invalid. It must be either a Scalar, Enum, Object, Interface, Union, " +
          "or a Non‐Null wrapper around one of those types. Notably, this field cannot return a list.")

    val edgeType = ObjectType[Ctx, E](
      name + "Edge",
      "An edge in a connection.",
      () =>
        List[Field[Ctx, E]](
          Field("node", nodeType, Some("The item at the end of the edge."), resolve = _.value.node),
          Field(
            "cursor",
            StringType,
            Some("A cursor for use in pagination."),
            resolve = _.value.cursor)
        ) ++ edgeFields
    )

    val connectionType = ObjectType[Ctx, Conn[Val]](
      name + "Connection",
      "A connection to a list of items.",
      () =>
        List[Field[Ctx, Conn[Val]]](
          Field(
            "pageInfo",
            pageInfoType.getOrElse(DefaultPageInfo.pageInfoType[Ctx, P]),
            Some("Information to aid in pagination."),
            resolve = ctx => connEv.pageInfo(ctx.value)
          ),
          Field(
            "edges",
            OptionType(ListType(OptionType(edgeType))),
            Some("A list of edges."),
            resolve = ctx => connEv.edges(ctx.value).map(Some(_)))
        ) ++ connectionFields
    )

    ConnectionDefinition[Ctx, Conn[Val], Val, E](edgeType, connectionType)
  }

  val CursorPrefix = "arrayconnection:"

  def empty[T] = DefaultConnection(PageInfo.empty, Vector.empty[Edge[T]])

  def connectionFromFutureSeq[T](seq: Future[Seq[T]], args: ConnectionArgs)(implicit
      ec: ExecutionContext): Future[Connection[T]] =
    seq.map(connectionFromSeq(_, args))

  def connectionFromSeq[T](seq: Seq[T], args: ConnectionArgs): Connection[T] =
    connectionFromSeq(seq, args, SliceInfo(0, seq.size))

  def connectionFromFutureSeq[T](seq: Future[Seq[T]], args: ConnectionArgs, sliceInfo: SliceInfo)(
      implicit ec: ExecutionContext): Future[Connection[T]] =
    seq.map(connectionFromSeq(_, args, sliceInfo))

  def connectionFromSeq[T](
      arraySlice: Seq[T],
      args: ConnectionArgs,
      sliceInfo: SliceInfo): Connection[T] = {
    val ConnectionArgs(beforeM, afterM, firstM, lastM) = args

    firstM.foreach(f =>
      if (f < 0)
        throw ConnectionArgumentValidationError("Argument 'first' must be a non-negative integer"))
    lastM.foreach(l =>
      if (l < 0)
        throw ConnectionArgumentValidationError("Argument 'last' must be a non-negative integer"))

    val SliceInfo(sliceOffset, totalItems) = sliceInfo

    val afterOffsetM = getOffset(afterM)
    val beforeOffsetM = getOffset(beforeM)

    // after based on before + last
    val backwardPaginationAfterMaybe = (beforeOffsetM, lastM) match {
      // (before - last) gives the index of first item to return but after cursor needs to be before it
      case (Some(before), Some(last)) => Some(before - last - 1)
      case (_, Some(last)) => Some(totalItems - last - 1)
      case _ => None
    }
    val finalAfterMaybe = List(backwardPaginationAfterMaybe, afterOffsetM).flatten.maxOption

    // before based on after + first
    val forwardPaginationBeforeMaybe = (afterOffsetM, firstM) match {
      // (after + first) gives the index of last item to return but before cursor needs to be after it
      case (Some(after), Some(first)) => Some(after + 1 + first)
      case (_, Some(first)) => Some(first)
      case _ => None
    }
    val finalBeforeMaybe = List(forwardPaginationBeforeMaybe, beforeOffsetM).flatten.minOption

    // align slice indices with all edges indices
    val sliceWithIdx = arraySlice.zipWithIndex.map { case (e, idx) => (e, idx + sliceOffset) }
    val trimmedSlice = sliceWithIdx.filter { case (_, idx) =>
      finalAfterMaybe.forall(_ < idx) && finalBeforeMaybe.forall(_ > idx)
    }

    // If result is empty and firstM and lastM are empty then no paging can be done
    if (trimmedSlice.isEmpty && firstM.isEmpty && lastM.isEmpty && beforeM.isEmpty && afterM.isEmpty)
      Connection.empty
    else {
      val edges = trimmedSlice.map { case (e, idx) => Edge(e, offsetToCursor(idx)) }
      val pageInfo = PageInfo(
        hasNextPage = (forwardPaginationBeforeMaybe, beforeOffsetM) match {
          case (Some(fpBefore), _) => fpBefore < beforeOffsetM.getOrElse(totalItems)
          case (_, Some(before)) => before < totalItems
          case _ => false
        },
        hasPreviousPage = (backwardPaginationAfterMaybe, afterOffsetM) match {
          case (Some(bpAfter), _) => bpAfter > afterOffsetM.getOrElse(-1)
          case (_, Some(after)) => after >= 0
          case _ => false
        },
        startCursor = edges.headOption.map(_.cursor),
        endCursor = edges.lastOption.map(_.cursor)
      )
      DefaultConnection(pageInfo, edges)
    }
  }

  def cursorForObjectInConnection[T, E](coll: Seq[T], obj: E): Option[String] = {
    val idx = coll.indexOf(obj)

    if (idx >= 0) Some(offsetToCursor(idx)) else None
  }

  private def getOffset(cursor: Option[String]): Option[Int] =
    cursor.flatMap(cursorToOffset)

  def offsetToCursor(offset: Int): String = Base64.encode(CursorPrefix + offset)

  def cursorToOffset(cursor: String): Option[Int] =
    GlobalId.fromGlobalId(cursor).flatMap(id => Try(id.id.toInt).toOption)
}

case class SliceInfo(sliceStart: Int, size: Int)

case class ConnectionDefinition[Ctx, Conn, Val, E <: Edge[Val]](
    edgeType: ObjectType[Ctx, E],
    connectionType: ObjectType[Ctx, Conn])

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
    endCursor: Option[String] = None)
    extends PageInfo

object DefaultPageInfo {
  def pageInfoType[Ctx, P <: PageInfo: ClassTag]: ObjectType[Ctx, P] =
    ObjectType[Ctx, P](
      "PageInfo",
      "Information about pagination in a connection.",
      () =>
        List[Field[Ctx, P]](
          Field(
            "hasNextPage",
            BooleanType,
            Some("When paginating forwards, are there more items?"),
            resolve = _.value.hasNextPage),
          Field(
            "hasPreviousPage",
            BooleanType,
            Some("When paginating backwards, are there more items?"),
            resolve = _.value.hasPreviousPage),
          Field(
            "startCursor",
            OptionType(StringType),
            Some("When paginating backwards, the cursor to continue."),
            resolve = _.value.startCursor),
          Field(
            "endCursor",
            OptionType(StringType),
            Some("When paginating forwards, the cursor to continue."),
            resolve = _.value.endCursor)
        )
    )
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

@implicitNotFound(
  "Type ${T} can't be used as a Connection. Please consider defining implicit instance of sangria.relay.ConnectionLike for type ${T} or extending sangria.relay.Connection trait.")
trait ConnectionLike[T[_], P <: PageInfo, Val, E <: Edge[Val]] {
  def pageInfo(conn: T[Val]): P
  def edges(conn: T[Val]): Seq[E]
}

object ConnectionLike {
  private object ConnectionIsConnectionLike
      extends ConnectionLike[Connection, PageInfo, Any, Edge[Any]] {
    override def pageInfo(conn: Connection[Any]) = conn.pageInfo
    override def edges(conn: Connection[Any]) = conn.edges
  }

  implicit def connectionIsConnectionLike[E <: Edge[Val], P <: PageInfo, Val, T[_]]
      : ConnectionLike[T, P, Val, E] =
    ConnectionIsConnectionLike.asInstanceOf[ConnectionLike[T, P, Val, E]]
}

case class ConnectionArgs(
    before: Option[String] = None,
    after: Option[String] = None,
    first: Option[Int] = None,
    last: Option[Int] = None)

object ConnectionArgs {
  def apply(args: WithArguments): ConnectionArgs =
    ConnectionArgs(
      args.arg(Connection.Args.Before),
      args.arg(Connection.Args.After),
      args.arg(Connection.Args.First),
      args.arg(Connection.Args.Last))

  val empty = ConnectionArgs()
}

case class ConnectionArgumentValidationError(message: String)
    extends Exception(message)
    with UserFacingError
