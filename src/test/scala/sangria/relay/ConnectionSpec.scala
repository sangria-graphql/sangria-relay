package sangria.relay

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.util.AwaitSupport
import sangria.schema._

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class ConnectionSpec extends WordSpec with Matchers with AwaitSupport {
  trait Pet {
    def name: String
  }

  case class Dog(name: String, tails: Int) extends Pet
  case class Cat(name: String, furColor: String) extends Pet

  case class User(name: String, pets: List[Pet] = Nil)

  class Repo {
    val Users = List(
      User("Dan"),
      User("Nick", pets = Cat("felix", "orange") :: Dog("bob", 2) :: Nil),
      User("Lee"),
      User("Joe"),
      User("Tim")
    )
  }

  val PetType = InterfaceType("Pet", fields[Repo, Pet](
    Field("name", StringType, resolve = _.value.name)
  ))

  val CatType = ObjectType("Cat", interfaces[Repo, Cat](PetType), fields[Repo, Cat](
    Field("furColor", StringType, resolve = _.value.furColor)
  ))

  val DogType = ObjectType("Dog", interfaces[Repo, Dog](PetType), fields[Repo, Dog](
    Field("tails", IntType, resolve = _.value.tails)
  ))

  val UserType: ObjectType[Repo, User] = ObjectType("User", () => fields(
    Field("name", OptionType(StringType), resolve = _.value.name),
    Field("pets", OptionType(petConnection), arguments = Connection.Args.All,
      resolve = ctx => Connection.connectionFromSeq(ctx.value.pets, ConnectionArgs(ctx))),
    Field("friends", OptionType(friendConnection), arguments = Connection.Args.All,
      resolve = ctx => Connection.connectionFromSeq(ctx.ctx.Users, ConnectionArgs(ctx)))
  ))

  val ConnectionDefinition(_, petConnection) =
    Connection.definition[Repo, Connection, Pet]("Pet", PetType)

  val ConnectionDefinition(_, scalarConnection) =
    Connection.definition[Repo, Connection, String]("Scalar", StringType)

  val ConnectionDefinition(_, friendConnection) = Connection.definition[Repo, Connection, User](
    name = "Friend",
    nodeType = UserType,
    edgeFields = fields(
      Field("friendshipTime", OptionType(StringType), resolve = _ => "Yesterday")),
    connectionFields = fields(
      Field("totalCount", OptionType(IntType), resolve = _.ctx.Users.size))
  )

  val QueryType = ObjectType("Query", fields[Repo, Unit](
    Field("scalars", OptionType(scalarConnection), arguments = Connection.Args.All,
      resolve = ctx => Connection.connectionFromSeq(Seq("foo", "bar"), ConnectionArgs(ctx))),
    Field("user", OptionType(UserType), resolve = _.ctx.Users.head)))

  val schema = Schema(QueryType, additionalTypes = CatType :: DogType :: Nil)

  "Connection.definition" should {
    "Includes connection and edge fields" in {
      val Success(doc) = QueryParser.parse(
        """
          query FriendsQuery {
            scalars(last: 1) {
              edges {
                node
              }
            }
            user {
              friends(first: 2) {
                totalCount
                pageInfo {
                  hasNextPage
                }
                edges {
                  friendshipTime
                  node {
                    name
                    pets(first: 2) {
                      edges {
                        node {
                          name
                          ... on Dog {
                            tails
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """)


      Executor.execute(schema, doc, userContext = new Repo).await should be(
        Map(
          "data" -> Map(
            "scalars" -> Map("edges" -> List(Map("node" -> "bar"))),
            "user" -> Map(
              "friends" -> Map(
                "totalCount" -> 5,
                "pageInfo" ->
                  Map(
                    "hasNextPage" -> true
                  ),
                "edges" -> List(
                  Map(
                    "friendshipTime" -> "Yesterday",
                    "node" -> Map(
                      "name" -> "Dan",
                      "pets" -> Map("edges" -> List())
                    )
                  ),
                  Map(
                    "friendshipTime" -> "Yesterday",
                    "node" -> Map(
                      "name" -> "Nick",
                      "pets" ->  Map(
                        "edges" -> List(
                          Map("node" -> Map("name" -> "felix")),
                          Map("node" -> Map("name" -> "bob", "tails" -> 2))
                        )
                      )
                    )
                  )
                )
              )
            ))))
    }

    "Not allow list node type" in {
      an [IllegalArgumentException] should be thrownBy
          Connection.definition[Repo, Connection, Seq[Pet]]("Pet", ListType(PetType))

      an [IllegalArgumentException] should be thrownBy
          Connection.definition[Repo, Connection, Option[Seq[Pet]]]("Pet", OptionType(ListType(PetType)))
    }
  }
}
