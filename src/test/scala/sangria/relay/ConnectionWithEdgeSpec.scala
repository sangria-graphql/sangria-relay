package sangria.relay

import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.util.AwaitSupport
import sangria.schema._

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConnectionWithEdgeSpec extends AnyWordSpec with Matchers with AwaitSupport {

  case class User(id: String, name: String)
  case class Account(id: String, number: String)
  case class Role(id: String, display: String)

  case class AccountUser(accountId: String, userId: String, roleId: String)
  case class UserEdge(node: User, cursor: String, roleId: String) extends Edge[User]

  class Repo {
    val cubert = User("001", "Cubert")
    val steve = User("002", "Steve")
    val hermes = User("003", "Hermes")

    val Users = cubert :: steve :: hermes :: Nil
    def getUser(userId: String): User = {
      Users.find(_.id == userId).get
    }

    val planetExpress = Account("Planet Express", "AO001")
    val planEx = Account("PlanEx", "AO002")
    val awesomeExpress = Account("Awesome Express", "AO003")

    val Accounts = planetExpress :: planEx :: awesomeExpress :: Nil

    val pilot = Role("pilot", "Pilot")
    val accountant = Role("accountant", "Accountant")
    val admin =  Role("admin", "Admin")

    val Roles = pilot :: accountant :: admin :: Nil

    def getRole(roleId: String): Role = {
      Roles.find(_.id == roleId).get
    }

    val AccountUsers = List(
      AccountUser(accountId = planetExpress.id, userId = hermes.id, roleId = accountant.id),
      AccountUser(accountId = planEx.id, userId = steve.id, roleId = admin.id),
      AccountUser(accountId = awesomeExpress.id, userId = cubert.id, roleId = pilot.id)
    )

    def getUsersForAccount(accountId: String): List[UserEdge] = {
      AccountUsers.filter(_.accountId == accountId).map(au =>
        UserEdge(node = getUser(au.userId), cursor = au.userId, roleId = au.roleId)
      )
    }
  }

  val UserType: ObjectType[Repo, User] = ObjectType("User", () => fields(
    Field("id", StringType, resolve = _.value.id),
    Field("name", StringType, resolve = _.value.name)
  ))

  val AccountType: ObjectType[Repo, Account] = ObjectType("Account", () => fields(
    Field("id", StringType, resolve = _.value.id),
    Field("number", StringType, resolve = _.value.number),
    Field("users", userConnection, arguments = Connection.Args.All,
      resolve = c => {
        val edges = c.ctx.getUsersForAccount(c.value.id)
        val firstEdge = edges.headOption
        val lastEdge = edges.lastOption

        DefaultConnection(
           PageInfo(
            startCursor = firstEdge map (_.cursor),
            endCursor = lastEdge map (_.cursor)
          ),
          edges
        )
      }
    )
  ))

  val RoleType: ObjectType[Repo, Role] = ObjectType("Role", () => fields(
    Field("id", StringType, resolve = _.value.id),
    Field("display", StringType, resolve = _.value.display)
  ))


  val ConnectionDefinition(_, userConnection) =
    Connection.definitionWithEdge[Repo, PageInfo, Connection, User, UserEdge](
      name = "User",
      nodeType = UserType,
      edgeFields = fields(
        Field("role", RoleType, resolve = c => c.ctx.getRole(c.value.roleId))
      )
    )

  val QueryType = ObjectType("Query", fields[Repo, Unit](
    Field("accounts", ListType(AccountType), resolve = _.ctx.Accounts))
  )

  val schema = Schema(QueryType, additionalTypes = AccountType :: UserType :: RoleType :: Nil)

  "Connection.definitionWithEdge" should {
    "Includes edge fields correctly" in {
      val Success(doc) = QueryParser.parse(
        """
          query AccountQuery {
            accounts {
              users {
                edges {
                  role {
                    display
                  }
                  node {
                    name
                  }
                }
              }
            }
          }
        """)

      val r = Executor.execute(schema, doc, userContext = new Repo).await

      r should be (
        Map(
          "data" -> Map(
            "accounts" -> List(
              Map(
                "users" -> Map(
                  "edges" -> List(
                    Map(
                      "role" -> Map(
                        "display" -> "Accountant"
                      ),
                      "node" -> Map(
                        "name" -> "Hermes"
                      )
                    )
                  )
                )
              ),
              Map(
                "users" -> Map(
                  "edges" -> List(
                    Map(
                      "role" -> Map(
                        "display" -> "Admin"
                      ),
                      "node" -> Map(
                        "name" -> "Steve"
                      )
                    )
                  )
                )
              ),
              Map(
                "users" -> Map(
                  "edges" -> List(
                    Map(
                      "role" -> Map(
                        "display" -> "Pilot"
                      ),
                      "node" -> Map(
                        "name" -> "Cubert"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }
}
