package sangria.relay

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.util.AwaitSupport
import sangria.schema._

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class GlobalIdSpec extends WordSpec with Matchers with AwaitSupport {
  case class User(id: String, name: String) extends Node
  case class Photo(photoId: String, width: Int)

  object Photo {
    implicit object PhotoId extends Identifiable[Photo] {
      def id(value: Photo) = value.photoId
    }
  }

  class Repo {
    val Users = List(
      User("1", "John Doe"),
      User("2", "Jane Smith")
    )

    val Photos = List(
      Photo("1", 300),
      Photo("2", 400)
    )
  }

  val NodeDefinition(nodeInterface, nodeField) = Node.definition((id: GlobalId, ctx: Context[Repo, Unit]) => {
    if (id.typeName == "User") ctx.ctx.Users.find(_.id == id.id)
    else ctx.ctx.Photos.find(_.photoId == id.id)
  }, Node.possibleNodeTypes[Repo, Node](UserType, PhotoType))

  val UserType: ObjectType[Unit, User] = ObjectType("User",
    fields[Unit, User](
      Node.globalIdField("User"),
      Field("name", OptionType(StringType), resolve = _.value.name)),
    interfaces[Unit, User](nodeInterface))

  val PhotoType: ObjectType[Unit, Photo] = ObjectType("Photo",
    fields[Unit, Photo](
      Node.globalIdField("Photo"),
      Field("width", OptionType(IntType), resolve = _.value.width)),
    interfaces[Unit, Photo](nodeInterface))

  val QueryType: ObjectType[Repo, Unit] = ObjectType("Query",
    fields[Repo, Unit](
      nodeField,
      Field("allObjects", OptionType(ListType(nodeInterface)),
        resolve = ctx => ctx.ctx.Users ++ ctx.ctx.Photos)))

  val schema = Schema(QueryType)

  "Global ID fields" should {
    "Gives different IDs" in {
      val Success(doc) = QueryParser.parse(
        """
          {
            allObjects {
              id
            }
          }
        """)

      Executor.execute(schema, doc, userContext = new Repo).await should be  (
        Map(
          "data" -> Map(
            "allObjects" -> List(
              Map("id" -> "VXNlcjox"),
              Map("id" -> "VXNlcjoy"),
              Map("id" -> "UGhvdG86MQ=="),
              Map("id" -> "UGhvdG86Mg==")))))
    }

    "Refetches the IDs" in {
      val Success(doc) = QueryParser.parse(
        """
          {
            user: node(id: "VXNlcjox") {
              id
              ... on User {
                name
              }
            },
            photo: node(id: "UGhvdG86MQ==") {
              id
              ... on Photo {
                width
              }
            }
          }
        """)

      Executor.execute(schema, doc, userContext = new Repo).await should be  (
        Map(
          "data" -> Map(
            "user" -> Map(
              "id" -> "VXNlcjox",
              "name" -> "John Doe"),
            "photo" -> Map(
              "id" -> "UGhvdG86MQ==",
              "width" -> 300))))
    }
  }
}
