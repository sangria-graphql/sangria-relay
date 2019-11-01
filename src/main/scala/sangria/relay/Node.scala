package sangria.relay

import sangria.execution.deferred.HasId
import sangria.marshalling.FromInput

import language.{existentials, implicitConversions}
import sangria.execution.{FieldTag, UserFacingError}
import sangria.schema._

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}

trait Node {
  def id: String
}

object Node {
  implicit def identifiableNodeType[Ctx, T](implicit ev: IdentifiableNode[Ctx, T]) = PossibleType.create[Node, T]

  val GlobalIdFieldName = "id"
  val GlobalIdFieldDescription = "The ID of an object"

  object Args {
    val Id = Argument("id", IDType, description = "The ID of an object")
    val Ids = Argument("ids", ListInputType(IDType), description = "The IDs of objects")
  }

  def definitionById[Ctx, Val, Res](
      resolve: (String, Context[Ctx, Val]) => LeafAction[Ctx, Option[Res]],
      possibleTypes: => List[PossibleNodeObject[Ctx, Node]] = Nil,
      tags: List[FieldTag] = Nil,
      complexity: Option[(Ctx, Args, Double) => Double] = None) = {
    val interfaceType = InterfaceType("Node", "An object with an ID", fields[Ctx, Res](
      Field("id", IDType, Some("The id of the object."), resolve = ctx =>
        possibleTypes.find(_.objectType.isInstanceOf(ctx.value)).map(_.id.asInstanceOf[IdentifiableNode[Ctx, Res]].id(ctx)).getOrElse(throw UnknownPossibleType(ctx.value)))
    ))

    val nodeField = Field("node", OptionType(interfaceType), Some("Fetches an object given its ID"),
      tags = tags,
      complexity = complexity,
      arguments = Args.Id :: Nil,
      possibleTypes = possibleTypes map (pt => PossibleObject(pt.objectType)),
      resolve = (ctx: Context[Ctx, Val]) => resolve(ctx.arg(Args.Id), ctx))

    val nodesField = Field("nodes", ListType(OptionType(interfaceType)), Some("Fetches objects given their IDs"),
      tags = tags,
      complexity = complexity,
      arguments = Args.Ids :: Nil,
      possibleTypes = possibleTypes map (pt => PossibleObject(pt.objectType)),
      resolve = (ctx: Context[Ctx, Val]) => Action.sequence(ctx.arg(Args.Ids).map(resolve(_, ctx))))

    NodeDefinition(interfaceType, nodeField, nodesField)
  }

  def definition[Ctx, Val, Res](
      resolve: (GlobalId, Context[Ctx, Val]) => LeafAction[Ctx, Option[Res]],
      possibleTypes: => List[PossibleNodeObject[Ctx, Node]] = Nil,
      tags: List[FieldTag] = Nil,
      complexity: Option[(Ctx, Args, Double) => Double] = None) =
    definitionById(
      (id: String, ctx: Context[Ctx, Val]) => resolve(GlobalId.fromGlobalId(id) getOrElse (throw WrongGlobalId(id)), ctx),
      possibleTypes,
      tags,
      complexity)

  def globalIdField[Ctx, Val](implicit ev: IdentifiableNode[Ctx, Val]) =
    Field(GlobalIdFieldName, IDType, Some(GlobalIdFieldDescription),
      resolve = (ctx: Context[Ctx, Val]) => GlobalId.toGlobalId(ctx.parentType.name, ev.id(ctx)))

  def globalIdField[Ctx, Val](
      typeName: Option[String] = None,
      tags: List[FieldTag] = Nil,
      complexity: Option[(Ctx, Args, Double) => Double] = None)(implicit ev: IdentifiableNode[Ctx, Val]) =
    Field(GlobalIdFieldName, IDType, Some(GlobalIdFieldDescription),
      tags = tags,
      complexity = complexity,
      resolve = (ctx: Context[Ctx, Val]) => GlobalId.toGlobalId(typeName.getOrElse(ctx.parentType.name), ev.id(ctx)))

  def pluralIdentifyingRootField[Ctx, Val, Res, Out, T](
    fieldName: String,
    fieldType: OutputType[Out],
    argName: String,
    argType: InputType[T],
    resolveSingleInput: (T, Context[Ctx, Val]) => Option[Out],
    description: Option[String] = None,
    tags: List[FieldTag] = Nil,
    complexity: Option[(Ctx, Args, Double) => Double] = None
  )(implicit res: ArgumentType[T], ev1: ValidOutType[Res, Out], fromInput: FromInput[T]) =
    Field(fieldName, OptionType(ListType(OptionType(fieldType))), description,
      tags = tags,
      complexity = complexity,
      arguments = Argument(argName, ListInputType(argType)) :: Nil,
      resolve = (ctx: Context[Ctx, Val]) => ctx.arg[Vector[T]](argName) map (resolveSingleInput(_, ctx)))

  def pluralIdentifyingRootFieldFut[Ctx, Val, Res, Out, T](
    fieldName: String,
    fieldType: OutputType[Out],
    argName: String,
    argType: InputType[T],
    resolveSingleInput: (T, Context[Ctx, Val]) => Future[Option[Out]],
    description: Option[String] = None,
    tags: List[FieldTag] = Nil,
    complexity: Option[(Ctx, Args, Double) => Double] = None
  )(implicit res: ArgumentType[T], ev1: ValidOutType[Res, Out], execCtx: ExecutionContext, fromInput: FromInput[T]) =
    Field(fieldName, OptionType(ListType(OptionType(fieldType))), description,
      tags = tags,
      complexity = complexity,
      arguments = Argument(argName, ListInputType(argType)) :: Nil,
      resolve = (ctx: Context[Ctx, Val]) => Future.sequence(ctx.arg[Seq[T]](argName) map (resolveSingleInput(_, ctx))))

  def possibleNodeTypes[Ctx, Abstract](objectTypes: PossibleNodeObject[Ctx, Abstract]*): List[PossibleNodeObject[Ctx, Abstract]] =
    objectTypes.toList
}

case class UnknownPossibleType(value: Any) extends IllegalArgumentException(s"Unknown node value of type '${value.getClass}'. It was not listed in possible types.")

case class WrongGlobalId(id: String) extends Exception(s"Invalid Global ID: $id") with UserFacingError

case class NodeDefinition[Ctx, Val, Res](interface: InterfaceType[Ctx, Res], nodeField: Field[Ctx, Val], nodeFields: Field[Ctx, Val])

@implicitNotFound("Type ${T} is not identifiable. Please consider defining implicit instance of sangria.relay.Identifiable or sangria.relay.IdentifiableNode for type ${T} or extending sangria.relay.Node trait.")
trait Identifiable[T] {
  def id(value: T): String
}

object Identifiable {
  private object IdentifiableEv extends Identifiable[Node] {
    override def id(value: Node) = value.id
  }

  implicit def identifiableEv[Ctx, T <: Node]: Identifiable[T] =
    IdentifiableEv.asInstanceOf[Identifiable[T]]

  implicit def identifiableEv[Ctx, T](implicit ev: HasId[T, String]): Identifiable[T] =
    new Identifiable[T] {
      def id(value: T) = ev.id(value)
    }
}

@implicitNotFound("Type ${T} is not identifiable. Please consider defining implicit instance of sangria.relay.Identifiable or sangria.relay.IdentifiableNode for type ${T} or extending sangria.relay.Node trait.")
trait IdentifiableNode[Ctx, T] {
  def id(ctx: Context[Ctx, T]): String
}

object IdentifiableNode extends IdentifiableNodeLowPrio {
  private object IdentifiableNodeEv extends IdentifiableNode[Any, Node] {
    override def id(ctx: Context[Any, Node]) = ctx.value.id
  }

  implicit def identifiableNodeEv[Ctx, T <: Node]: IdentifiableNode[Ctx, T] =
    IdentifiableNodeEv.asInstanceOf[IdentifiableNode[Ctx, T]]

  implicit def identifiableNodeIdEv[Ctx, T : Identifiable]: IdentifiableNode[Ctx, T] = new IdentifiableNode[Ctx, T] {
    lazy val identifiable = implicitly[Identifiable[T]]
    def id(ctx: Context[Ctx, T]) = identifiable.id(ctx.value)
  }
}

trait IdentifiableNodeLowPrio {
  implicit def identifiableNodeCtxEv[Ctx1, Ctx2, T](implicit ev: IdentifiableNode[Ctx1, T], ev1: Ctx2 <:< Ctx1): IdentifiableNode[Ctx2, T] =
    ev.asInstanceOf[IdentifiableNode[Ctx2, T]]
}

case class PossibleNodeObject[Ctx, Abstract] private (objectType: ObjectType[Ctx, _], id: IdentifiableNode[_, _])

object PossibleNodeObject {
  implicit def apply[Ctx, Abstract, Concrete](obj: ObjectType[Ctx, Concrete])(implicit ev: PossibleType[Abstract, Concrete], id: IdentifiableNode[Ctx, Concrete]): PossibleNodeObject[Ctx, Abstract] =
    PossibleNodeObject[Ctx, Abstract](obj, id)

  implicit def applyUnit[Ctx, Abstract, Concrete](obj: ObjectType[Unit, Concrete])(implicit ev: PossibleType[Abstract, Concrete], id: IdentifiableNode[Ctx, Concrete]): PossibleNodeObject[Ctx, Abstract] =
    PossibleNodeObject[Ctx, Abstract](obj, id)
}
