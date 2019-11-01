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
    implicit object PostId extends IdentifiableNode[PostRepo, Post] {
      def id(ctx: Context[PostRepo, Post]) = ctx.value.postId.toString
    }
  }

  trait PhotoRepo {
    def Photos: List[Photo]
  }

  trait PostRepo {
    def Posts: List[Post]
  }

  class Repo extends PhotoRepo with PostRepo {
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

  val NodeDefinition(nodeInterface, nodeField, nodesField) = Node.definition((id: GlobalId, ctx: Context[Repo, Unit]) => {
    if (id.typeName == "User") ctx.ctx.Users.find(_.id == id.id)
    else if (id.typeName == "CustomPhoto") ctx.ctx.Photos.find(_.photoId == id.id)
    else ctx.ctx.Posts.find(_.postId == id.id.toInt)
  }, Node.possibleNodeTypes[Repo, Node](UserType, PhotoType, PostType))

  val UserType: ObjectType[Unit, User] = ObjectType("User", interfaces[Unit, User](nodeInterface),
    fields[Unit, User](
      Node.globalIdField,
      Field("name", OptionType(StringType), resolve = _.value.name)))

  val PhotoType: ObjectType[Repo, Photo] = ObjectType("Photo", interfaces[Repo, Photo](nodeInterface),
    fields[Repo, Photo](
      Node.globalIdField(Some("CustomPhoto")),
      Field("width", OptionType(IntType), resolve = _.value.width)))

  val PostType: ObjectType[Repo, Post] = ObjectType("Post", interfaces[Repo, Post](nodeInterface),
    fields[Repo, Post](
      Node.globalIdField,
      Field("text", OptionType(StringType), resolve = _.value.text)))

  val QueryType: ObjectType[Repo, Unit] = ObjectType("Query",
    fields[Repo, Unit](
      nodeField,
      nodesField,
      Field("allObjects", OptionType(ListType(nodeInterface)),
        resolve = ctx => ctx.ctx.Users ++ ctx.ctx.Photos ++ ctx.ctx.Posts)))

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
              Map("id" -> "Q3VzdG9tUGhvdG86MQ=="),
              Map("id" -> "Q3VzdG9tUGhvdG86Mg=="),
              Map("id" -> "UG9zdDox"),
              Map("id" -> "UG9zdDoy")))))
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
          "data" -> Map(
            "user" -> Map(
              "id" -> "VXNlcjox",
              "name" -> "John Doe"),
            "photo" -> Map(
              "id" -> "Q3VzdG9tUGhvdG86MQ==",
              "width" -> 300),
            "post" -> Map(
              "id" -> "UG9zdDoy",
              "text" -> "ipsum"))))
    }
  }

  "GlobalId.fromGlobalId" should {
    "return None for empty IDs" in {
      GlobalId.fromGlobalId("") shouldBe empty
    }

    "return None for incorrect Base64 IDs" in {
      GlobalId.fromGlobalId("123") shouldBe empty
    }

    "return None for IDs where the decoded value does not contain a colon" in {
      GlobalId.fromGlobalId("UG9zdA==") shouldBe empty
    }

    "return None for IDs where the decoded value does contain a colon at the end" in {
      GlobalId.fromGlobalId("UG9zdDo=") shouldBe empty
    }
  }
}
