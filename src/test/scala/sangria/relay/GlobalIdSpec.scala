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
  case class Post(postId: Int, text: String)

  object Photo {
    implicit object PhotoId extends Identifiable[Photo] {
      def id(value: Photo) = value.photoId
    }
  }

  object Post {
    implicit object PhotoId extends Identifiable[Post] {
      def id(value: Post) = value.postId.toString
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

    val Posts = List(
      Post(1, "lorem"),
      Post(2, "ipsum")
    )
  }

  val NodeDefinition(nodeInterface, nodeField) = Node.definition((id: GlobalId, ctx: Context[Repo, Unit]) ⇒ {
    if (id.typeName == "User") ctx.ctx.Users.find(_.id == id.id)
    else if (id.typeName == "CustomPhoto") ctx.ctx.Photos.find(_.photoId == id.id)
    else ctx.ctx.Posts.find(_.postId == id.id.toInt)
  }, Node.possibleNodeTypes[Repo, Node](UserType, PhotoType, PostType))

  val UserType: ObjectType[Unit, User] = ObjectType("User", interfaces[Unit, User](nodeInterface),
    fields[Unit, User](
      Node.globalIdField,
      Field("name", OptionType(StringType), resolve = _.value.name)))

  val PhotoType: ObjectType[Unit, Photo] = ObjectType("Photo", interfaces[Unit, Photo](nodeInterface),
    fields[Unit, Photo](
      Node.globalIdField(Some("CustomPhoto")),
      Field("width", OptionType(IntType), resolve = _.value.width)))

  val PostType: ObjectType[Unit, Post] = ObjectType("Post", interfaces[Unit, Post](nodeInterface),
    fields[Unit, Post](
      Node.globalIdField,
      Field("text", OptionType(StringType), resolve = _.value.text)))

  val QueryType: ObjectType[Repo, Unit] = ObjectType("Query",
    fields[Repo, Unit](
      nodeField,
      Field("allObjects", OptionType(ListType(nodeInterface)),
        resolve = ctx ⇒ ctx.ctx.Users ++ ctx.ctx.Photos ++ ctx.ctx.Posts)))

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
          "data" → Map(
            "allObjects" → List(
              Map("id" → "VXNlcjox"),
              Map("id" → "VXNlcjoy"),
              Map("id" → "Q3VzdG9tUGhvdG86MQ=="),
              Map("id" → "Q3VzdG9tUGhvdG86Mg=="),
              Map("id" → "UG9zdDox"),
              Map("id" → "UG9zdDoy")))))
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
            }
            photo: node(id: "Q3VzdG9tUGhvdG86MQ==") {
              id
              ... on Photo {
                width
              }
            }
            post: node(id: "UG9zdDoy") {
              id
              ... on Post {
                text
              }
            }
          }
        """)

      Executor.execute(schema, doc, userContext = new Repo).await should be  (
        Map(
          "data" → Map(
            "user" → Map(
              "id" → "VXNlcjox",
              "name" → "John Doe"),
            "photo" → Map(
              "id" → "Q3VzdG9tUGhvdG86MQ==",
              "width" → 300),
            "post" → Map(
              "id" → "UG9zdDoy",
              "text" → "ipsum"))))
    }
  }
}