package sangria.relay

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.util.{AwaitSupport, DebugUtil, ResultHelper}
import sangria.schema._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class NodeSpec extends WordSpec with Matchers with AwaitSupport with ResultHelper {
  case class User(id: String, name: String) extends Node
  case class Photo(photoId: String, width: Int)

  object Photo {
    implicit object PhotoId extends Identifiable[Photo] {
      def id(photo: Photo) = photo.photoId
    }
  }

  class Repo {
    val Users = List(
      User("1", "John Doe"),
      User("2", "Jane Smith")
    )

    val Photos = List(
      Photo("3", 300),
      Photo("4", 400)
    )
  }

  val NodeDefinition(nodeInterface, nodeField, nodesField) = Node.definitionById((id: String, ctx: Context[Repo, Unit]) => {
    if (ctx.ctx.Users exists (_.id == id)) ctx.ctx.Users.find(_.id == id)
    else ctx.ctx.Photos.find(_.photoId == id)
  }, Node.possibleNodeTypes[Repo, Node](UserType, PhotoType))

  val UserType: ObjectType[Unit, User] = ObjectType("User", interfaces[Unit, User](nodeInterface),
    fields[Unit, User](
      Field("id", IDType, resolve = _.value.id),
      Field("name", OptionType(StringType), resolve = _.value.name)))

  val PhotoType: ObjectType[Unit, Photo] = ObjectType("Photo", interfaces[Unit, Photo](nodeInterface),
    fields[Unit, Photo](
      Field("id", IDType, resolve = _.value.photoId),
      Field("width", OptionType(IntType), resolve = _.value.width)))

  val QueryType: ObjectType[Repo, Unit] = ObjectType("Query", fields[Repo, Unit](nodeField, nodesField))

  val schema = Schema(QueryType)

  "Node interface and fields" should {
    "Allows refetching" should {
      "Gets the correct ID for users" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              node(id: "1") {
                id
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "node" -> Map("id" -> "1"))))
      }

      "gets the correct IDs for users" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              nodes(ids: ["1", "2"]) {
                id
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "nodes" -> List(Map("id" -> "1"), Map("id" -> "2")))))
      }

      "Gets the correct ID for photos" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              node(id: "4") {
                id
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "node" -> Map("id" -> "4"))))
      }

      "Gets the correct width for photos" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              node(id: "4") {
                id
                ... on Photo {
                  width
                }
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "node" -> Map(
                "id" -> "4",
                "width" -> 400))))
      }

      "gets the correct IDs for photos" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              nodes(ids: ["3", "4"]) {
                id
                ... on Photo {
                  width
                }
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "nodes" -> List(
                Map("id" -> "3", "width" -> 300),
                Map("id" -> "4", "width" -> 400)))))
      }

      "gets the correct IDs for multiple types" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              nodes(ids: ["1", "3"]) {
                id

                ... on Photo {
                  width
                }

                ... on User {
                  name
                }
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "nodes" -> List(
                Map("id" -> "1", "name" -> "John Doe"),
                Map("id" -> "3", "width" -> 300)))))
      }

      "Gets the correct type name for users" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              node(id: "1") {
                id
                __typename
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "node" -> Map(
                "id" -> "1",
                "__typename" -> "User"))))
      }

      "Gets the correct type name for photos" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              node(id: "4") {
                id
                __typename
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "node" -> Map(
                "id" -> "4",
                "__typename" -> "Photo"))))
      }

      "Ignores photo fragments on user" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              node(id: "1") {
                id
                ... on Photo {
                  width
                }
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "node" -> Map(
                "id" -> "1"))))
      }

      "Returns null for bad IDs" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              node(id: "5") {
                id
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "node" -> null)))
      }

      "Returns nulls for bad IDs" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              nodes(ids: ["3", "5"]) {
                id
              }
            }
          """)


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "nodes" -> List(Map("id" -> "3"), null))))
      }
    }

    "Correctly introspects" should {
      "Lists all relevant abstract and concrete types" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              __schema {
                types {
                  name
                }
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await
          .getProp("data").getProp("__schema").getProp("types").asList.map(_.getProp("name")) should (
            contain("Node") and contain("User") and contain("Photo"))
      }

      "Has correct node interface" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              __type(name: "Node") {
                name
                kind
                fields {
                  name
                  type {
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


        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "__type" -> Map(
                "name" -> "Node",
                "kind" -> "INTERFACE",
                "fields" -> List(
                  Map(
                    "name" -> "id",
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "ofType" -> Map(
                        "name" -> "ID",
                        "kind" -> "SCALAR"
                      )
                    )
                  )
                )
              ))))
      }

      "Has correct node root field" in {
        val Success(doc) = QueryParser.parse(
          """
            {
              __schema {
                queryType {
                  fields {
                    name
                    type {
                      name
                      kind
                    }
                    args {
                      name
                      type {
                        kind
                        ofType {
                          name
                          kind
                        }
                      }
                    }
                  }
                }
              }
            }
          """)

        Executor.execute(schema, doc, userContext = new Repo).await should be (
          Map(
            "data" -> Map(
              "__schema" -> Map(
                "queryType" -> Map(
                  "fields" -> List(
                    Map(
                      "name" -> "node",
                      "type" -> Map(
                        "name" -> "Node",
                        "kind" -> "INTERFACE"),
                      "args" -> Vector(
                        Map(
                          "name" -> "id",
                          "type" -> Map(
                            "kind" -> "NON_NULL",
                            "ofType" -> Map(
                              "name" -> "ID",
                              "kind" -> "SCALAR"))))),
                    Map(
                      "name" -> "nodes",
                      "type" -> Map(
                        "name" -> null,
                        "kind" -> "NON_NULL"),
                      "args" -> Vector(
                        Map(
                          "name" -> "ids",
                          "type" -> Map(
                            "kind" -> "NON_NULL",
                            "ofType" -> Map(
                              "name" -> null,
                              "kind" -> "LIST")))))
                  )
                )
              ))))
      }
    }
  }
}
