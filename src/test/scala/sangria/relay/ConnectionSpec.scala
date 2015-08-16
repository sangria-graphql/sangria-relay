package sangria.relay

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.util.AwaitSupport
import sangria.schema._

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class ConnectionSpec extends WordSpec with Matchers with AwaitSupport {
  case class User(name: String)

  class Repo {
    val Users = List(
      User("Dan"),
      User("Nick"),
      User("Lee"),
      User("Joe"),
      User("Tim")
    )
  }

  val UserType: ObjectType[Repo, User] = ObjectType("User", () => fields(
    Field("name", OptionType(StringType), resolve = _.value.name),
    Field("friends", OptionType(friendConnection), arguments = Connection.Args.All,
      resolve = ctx => Connection.connectionFromSeq(ctx.ctx.Users map (Some(_)), ConnectionArgs(ctx)))
  ))

  val ConnectionDefinition(_, friendConnection) = Connection.definition[Repo, Connection, User](
    name = "Friend",
    nodeType = UserType,
    edgeFields = fields(
      Field("friendshipTime", OptionType(StringType), resolve = _ => "Yesterday")),
    connectionFields = fields(
      Field("totalCount", OptionType(IntType), resolve = _.ctx.Users.size))
  )

  val QueryType = ObjectType("Query", fields[Repo, Unit](
    Field("user", OptionType(UserType), resolve = _.ctx.Users.head)))

  val schema = Schema(QueryType)

  "Connection.definition" should {
    "Includes connection and edge fields" in {
      val Success(doc) = QueryParser.parse(
        """
          query FriendsQuery {
            user {
              friends(first: 2) {
                totalCount
                edges {
                  friendshipTime
                  node {
                    name
                  }
                }
              }
            }
          }
        """)


      Executor.execute(schema, doc, userContext = new Repo).await should be(
        Map(
          "data" -> Map(
            "user" -> Map(
              "friends" -> Map(
                "totalCount" -> 5,
                "edges" -> List(
                  Map(
                    "friendshipTime" -> "Yesterday",
                    "node" -> Map(
                      "name" -> "Dan"
                    )
                  ),
                  Map(
                    "friendshipTime" -> "Yesterday",
                    "node" -> Map(
                      "name" -> "Nick"
                    )
                  )
                )
              )
            ))))
    }
  }
}
