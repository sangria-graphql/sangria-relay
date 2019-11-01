package sangria.relay

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.{ErrorWithResolver, Executor}
import sangria.marshalling.{CoercedScalaResultMarshaller, ResultMarshaller, FromInput}
import sangria.parser.QueryParser
import sangria.relay.util.{ResultHelper, AwaitSupport}
import sangria.schema._

import scala.concurrent.Future
import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class MutationSpec extends WordSpec with Matchers with AwaitSupport with ResultHelper {
  case class Counter(id: String, num: Int)

  object Counter {
    implicit object CounterMutation extends MutationLike[Counter] {
      override def clientMutationId(value: Counter) = Some(value.id)
    }

    implicit object CounterFromInput extends FromInput[Counter] {
      val marshaller = CoercedScalaResultMarshaller.default
      def fromResult(node: marshaller.Node) = {
        val input = node.asInstanceOf[Map[String, Any]]

        Counter(
          id = input(Mutation.ClientMutationIdFieldName).asInstanceOf[Option[String]].get,
          num = input get "num" flatMap (_.asInstanceOf[Option[Int]]) getOrElse 1)
      }
    }
  }

  class Repo {
    def mutateCounter(c: Counter) = c.copy(num = c.num + 1)
  }

  val simpleMutation = Mutation.fieldWithClientMutationId[Repo, Unit, Counter, Counter](
    fieldName = "simpleMutation",
    typeName = "SimpleMutation",
    inputFields = List(InputField("num", OptionInputType(IntType))),
    outputFields = fields(Field("num", OptionType(IntType), resolve = _.value.num)),
    mutateAndGetPayload = (counter, ctx) => ctx.ctx.mutateCounter(counter)
  )

  val simpleFutureMutation = Mutation.fieldWithClientMutationId[Repo, Unit, Counter, Counter](
    fieldName = "simpleFutureMutation",
    typeName = "SimpleFutureMutation",
    inputFields = List(InputField("num", OptionInputType(IntType))),
    outputFields = fields(Field("num", OptionType(IntType), resolve = _.value.num)),
    mutateAndGetPayload = (counter, ctx) => Future.successful(ctx.ctx.mutateCounter(counter))
  )

  val mutation = ObjectType("Mutation",
    fields[Repo, Unit](simpleMutation, simpleFutureMutation))

  val schema = Schema(mutation, Some(mutation))

  "fieldWithClientMutationId" when {
    "Behaves correctly" should {
      "Requires an argument" in {
        val Success(doc) = QueryParser.parse(
          """
            mutation M {
              simpleMutation {
                num
              }
            }
          """)

        val result = Executor.execute(schema, doc, userContext = new Repo).recover {
          case e: ErrorWithResolver => e.resolveError
        }.await

        result.getProp("data").asAnyRef should equal (null)

        val errors = result.getProp("errors").asList

        errors should have size 1

        errors(0).getProp("message").asString should include ("Field 'simpleMutation' argument 'input' of type 'SimpleMutationInput!' is required but not provided.")
      }

      "Returns the same client mutation ID" in {
        val Success(doc) = QueryParser.parse(
          """
            mutation M {
              simpleMutation(input: {clientMutationId: "abc"}) {
                num
                clientMutationId
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be  (
          Map(
            "data" -> Map(
              "simpleMutation" -> Map(
                "num" -> 2,
                "clientMutationId" -> "abc"))))
      }

      "Accepts num argument" in {
        val Success(doc) = QueryParser.parse(
          """
            mutation M {
              simpleMutation(input: {clientMutationId: "abc", num: 46}) {
                num
                clientMutationId
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be  (
          Map(
            "data" -> Map(
              "simpleMutation" -> Map(
                "num" -> 47,
                "clientMutationId" -> "abc"))))
      }

      "Supports promise mutations" in {
        val Success(doc) = QueryParser.parse(
          """
            mutation M {
              simpleFutureMutation(input: {clientMutationId: "abc"}) {
                num
                clientMutationId
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be  (
          Map(
            "data" -> Map(
              "simpleFutureMutation" -> Map(
                "num" -> 2,
                "clientMutationId" -> "abc"))))
      }
    }

    "Introspects correctly" should {
      "Supports promise mutations" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              __type(name: "SimpleMutationInput") {
                name
                kind
                inputFields {
                  name
                  type {
                    name
                    kind
                    ofType {
                      name
                      kind
                    }
                  }
                }
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be  (
          Map(
            "data" -> Map(
              "__type" -> Map(
                "name" -> "SimpleMutationInput",
                "kind" -> "INPUT_OBJECT",
                "inputFields" -> List(
                  Map(
                    "name" -> "num",
                    "type" -> Map(
                      "name" -> "Int",
                      "kind" -> "SCALAR",
                      "ofType" -> null
                    )
                  ),
                  Map(
                    "name" -> "clientMutationId",
                    "type" -> Map(
                      "name" -> "String",
                      "kind" -> "SCALAR",
                      "ofType" -> null
                    )
                  )
                )
              ))))
      }

      "Contains correct payload" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              __type(name: "SimpleMutationPayload") {
                name
                kind
                fields {
                  name
                  type {
                    name
                    kind
                    ofType {
                      name
                      kind
                    }
                  }
                }
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be  (
          Map(
            "data" -> Map(
              "__type" -> Map(
                "name" -> "SimpleMutationPayload",
                "kind" -> "OBJECT",
                "fields" -> List(
                  Map(
                    "name" -> "num",
                    "type" -> Map(
                      "name" -> "Int",
                      "kind" -> "SCALAR",
                      "ofType" -> null
                    )
                  ),
                  Map(
                    "name" -> "clientMutationId",
                    "type" -> Map(
                      "name" -> "String",
                      "kind" -> "SCALAR",
                      "ofType" -> null
                    )
                  )
                )
              ))))
      }

      "Contains correct field" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              __schema {
                mutationType {
                  fields {
                    name
                    args {
                      name
                      type {
                        name
                        kind
                        ofType {
                          name
                          kind
                        }
                      }
                    }
                    type {
                      name
                      kind
                    }
                  }
                }
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be  (
          Map(
            "data" -> Map(
              "__schema" -> Map(
                "mutationType" -> Map(
                  "fields" -> List(
                    Map(
                      "name" -> "simpleMutation",
                      "args" -> List(
                        Map(
                          "name" -> "input",
                          "type" -> Map(
                            "name" -> null,
                            "kind" -> "NON_NULL",
                            "ofType" -> Map(
                              "name" -> "SimpleMutationInput",
                              "kind" -> "INPUT_OBJECT"
                            )
                          )
                        )
                      ),
                      "type" -> Map(
                        "name" -> "SimpleMutationPayload",
                        "kind" -> "OBJECT"
                      )
                    ),
                    Map(
                      "name" -> "simpleFutureMutation",
                      "args" -> List(
                        Map(
                          "name" -> "input",
                          "type" -> Map(
                            "name" -> null,
                            "kind" -> "NON_NULL",
                            "ofType" -> Map(
                              "name" -> "SimpleFutureMutationInput",
                              "kind" -> "INPUT_OBJECT"
                            )
                          )
                        )
                      ),
                      "type" -> Map(
                        "name" -> "SimpleFutureMutationPayload",
                        "kind" -> "OBJECT"
                      )
                    )
                  )
                )
              ))))
      }
    }
  }
}