package sangria.relay

import sangria.schema._

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.reflect.ClassTag

trait Mutation {
  def clientMutationId: String
}

object Mutation {
  val ClientMutationIdFieldName = "clientMutationId"

  def fieldWithClientMutationId[Ctx, Val, Res : MutationLike : ClassTag](
        fieldName: String,
        typeName: String,
        mutateAndGetPayload: (InputObjectType.InputObjectRes, Context[Ctx, Val]) => Res,
        inputFields: List[InputField[_]] = Nil,
        outputFields: List[Field[Ctx, Res]] = Nil) =
    fieldWithClientMutationIdFut(fieldName, typeName, mutateAndGetPayload =
      (in: InputObjectType.InputObjectRes, ctx: Context[Ctx, Val]) =>
        Future.successful(mutateAndGetPayload(in, ctx)), inputFields, outputFields)

  def fieldWithClientMutationIdFut[Ctx, Val, Res : MutationLike : ClassTag](
        fieldName: String,
        typeName: String,
        mutateAndGetPayload: (InputObjectType.InputObjectRes, Context[Ctx, Val]) => Future[Res],
        inputFields: List[InputField[_]] = Nil,
        outputFields: List[Field[Ctx, Res]] = Nil) = {
    val inputType = InputObjectType(typeName + "Input",
      fields = inputFields :+ InputField(ClientMutationIdFieldName, StringType))

    val outputType = ObjectType(typeName + "Payload",
      outputFields :+ Field(ClientMutationIdFieldName, StringType, resolve = (ctx: Context[Ctx, Res]) => implicitly[MutationLike[Res]].clientMutationId(ctx.value)))

    val inputArg = Argument("input", inputType)

    Field(fieldName, OptionType(outputType), arguments = inputArg :: Nil, resolve = (ctx: Context[Ctx, Val]) => mutateAndGetPayload(ctx.arg(inputArg), ctx))
  }
}

@implicitNotFound("Type ${T} can't be used as a Mutation. Please consider defining implicit instance of sangria.relay.MutationLike for type ${T} or extending sangria.relay.Mutation trait.")
trait MutationLike[T] {
  def clientMutationId(value: T): String
}

object MutationLike {
  private object MutationIsMutationLike extends MutationLike[Mutation] {
    override def clientMutationId(value: Mutation) = value.clientMutationId
  }

  implicit def MutationIsMutationLike[T <: Mutation]: MutationLike[T] =
    MutationIsMutationLike.asInstanceOf[MutationLike[T]]
}