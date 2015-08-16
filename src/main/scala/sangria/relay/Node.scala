package sangria.relay

import language.{implicitConversions, existentials}

import sangria.execution.{UserFacingError, ExecutionError}

import sangria.schema._
import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}

trait Node {
  def id: String
}

object Node {
  implicit def identifiableNodeType[T: Identifiable] = PossibleType.create[Node, T]

  object Args {
    val ID = Argument("id", IDType, description = "The ID of an object")

    val All = ID :: Nil
  }

  def definitionById[Ctx, Val, Res](resolve: (String, Context[Ctx, Val]) => Action[Ctx, Option[Res]], possibleTypes: => List[PossibleNodeObject[Ctx, Node]] = Nil) = {
    val interfaceType = InterfaceType("Node", "An object with an ID", fields[Ctx, Res](
      Field("id", IDType, Some("The id of the object."), resolve = ctx =>
        possibleTypes.find(_.objectType.isInstanceOf(ctx.value)).map(_.id.asInstanceOf[Identifiable[Res]].id(ctx.value)).getOrElse(throw UnknownPossibleType(ctx.value)))
    ))

    val nodeField = Field("node", OptionType(interfaceType), Some("Fetches an object given its ID"),
      arguments = Args.All,
      possibleTypes = possibleTypes map (pt => PossibleObject(pt.objectType)),
      resolve = (ctx: Context[Ctx, Val]) => resolve(ctx.arg(Args.ID), ctx))

    NodeDefinition(interfaceType, nodeField)
  }

  def definition[Ctx, Val, Res](resolve: (GlobalId, Context[Ctx, Val]) => Action[Ctx, Option[Res]], possibleTypes:  => List[PossibleNodeObject[Ctx, Node]] = Nil) =
    definitionById((id: String, ctx: Context[Ctx, Val]) => resolve(GlobalId.fromGlobalId(id) getOrElse (throw WrongGlobalId(id)), ctx), possibleTypes)

  def globalIdField[Ctx, Val : Identifiable](typeName: String) =
    Field("id", IDType, Some("The ID of an object"),
      resolve = (ctx: Context[Ctx, Val]) => GlobalId.toGlobalId(typeName, implicitly[Identifiable[Val]].id(ctx.value)))

  def pluralIdentifyingRootField[Ctx, Val, Res, Out, T](
    fieldName: String,
    fieldType: OutputType[Out],
    argName: String,
    argType: InputType[T],
    resolveSingleInput: (T, Context[Ctx, Val]) => Option[Out],
    description: Option[String] = None
  )(implicit res: ArgumentType[T], ev1: ValidOutType[Res, Out]) =
    Field(fieldName, OptionType(ListType(OptionType(fieldType))), description,
      arguments = Argument(argName, ListInputType(argType)) :: Nil,
      resolve = (ctx: Context[Ctx, Val]) => ctx.arg[List[T]](argName) map (resolveSingleInput(_, ctx)))

  def pluralIdentifyingRootFieldFut[Ctx, Val, Res, Out, T](
    fieldName: String,
    fieldType: OutputType[Out],
    argName: String,
    argType: InputType[T],
    resolveSingleInput: (T, Context[Ctx, Val]) => Future[Option[Out]],
    description: Option[String] = None
  )(implicit res: ArgumentType[T], ev1: ValidOutType[Res, Out], execCtx: ExecutionContext) =
    Field(fieldName, OptionType(ListType(OptionType(fieldType))), description,
      arguments = Argument(argName, ListInputType(argType)) :: Nil,
      resolve = (ctx: Context[Ctx, Val]) => Future.sequence(ctx.arg[List[T]](argName) map (resolveSingleInput(_, ctx))))

  def possibleNodeTypes[Ctx, Abstract](objectTypes: PossibleNodeObject[Ctx, Abstract]*): List[PossibleNodeObject[Ctx, Abstract]] =
    objectTypes.toList
}

case class UnknownPossibleType(value: Any) extends IllegalArgumentException(s"Unknown node value of type '${value.getClass}'. It was not listed in possible types.")

case class WrongGlobalId(id: String) extends Exception(s"Invalid Global ID: $id") with UserFacingError

case class NodeDefinition[Ctx, Val, Res](interface: InterfaceType[Ctx, Res], field: Field[Ctx, Val])

@implicitNotFound("Type ${T} is not identifiable. Please consider defining implicit instance of Identifiable for type ${T} or extending sangria.relay.Node trait.")
trait Identifiable[T] {
  def id(value: T): String
}

object Identifiable {
  private object IdentifiableNode extends Identifiable[Node] {
    override def id(node: Node) = node.id
  }

  implicit def identifiableNode[T <: Node]: Identifiable[T] =
    IdentifiableNode.asInstanceOf[Identifiable[T]]
}

case class PossibleNodeObject[Ctx, Abstract] private (objectType: ObjectType[Ctx, _], id: Identifiable[_])

object PossibleNodeObject {
  implicit def apply[Ctx, Abstract, Concrete](obj: ObjectType[Ctx, Concrete])(implicit ev: PossibleType[Abstract, Concrete], id: Identifiable[Concrete]): PossibleNodeObject[Ctx, Abstract] =
    PossibleNodeObject[Ctx, Abstract](obj, id)

  implicit def applyUnit[Ctx, Abstract, Concrete](obj: ObjectType[Unit, Concrete])(implicit ev: PossibleType[Abstract, Concrete], id: Identifiable[Concrete]): PossibleNodeObject[Ctx, Abstract] =
    PossibleNodeObject[Ctx, Abstract](obj.asInstanceOf[ObjectType[Ctx, Concrete]], id)
}
