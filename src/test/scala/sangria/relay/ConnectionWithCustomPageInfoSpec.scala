package sangria.relay

import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.util.AwaitSupport
import sangria.schema._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConnectionWithCustomPageInfoSpec extends AnyWordSpec with Matchers with AwaitSupport {

  /**
   * custom PageInfo to be included in Connection
   */
  case class CustomPageInfo(
    currentPage: Int,
    hasNextPage: Boolean = false,
    hasPreviousPage: Boolean = false,
    startCursor: Option[String] = None,
    endCursor: Option[String] = None) extends PageInfo

  /**
   * custom Connection includes CustomPageInfo
   */
  case class CustomConnection[T](
    pageInfo: CustomPageInfo,
    edges: Seq[Edge[T]])

  implicit def customConnectionLike[T] = new ConnectionLike[CustomConnection, CustomPageInfo, T, Edge[T]] {
    override def pageInfo(conn: CustomConnection[T]): CustomPageInfo = conn.pageInfo
    override def edges(conn: CustomConnection[T]): Seq[Edge[T]] = conn.edges
  }


  case class User(id: String, name: String)
  case class Task(id: String, userId: String, description: String)

  case class AccountUser(accountId: String, userId: String, roleId: String)

  class Repo {
    private val cubert = User("001", "Cubert")
    private val steve = User("002", "Steve")

    val Users = cubert :: steve :: Nil

    private val pencil = Task("00a", "001", "buy pencil")
    private val milk = Task("00b", "001", "buy milk")

    private val apple = Task("00c", "002", "buy apple")
    private val banana = Task("00d", "002", "buy banana")
    private val orange = Task("00e", "002", "buy orange")
    private val grape = Task("00f", "002", "buy grape")
    private val strawberry = Task("00g", "002", "buy strawberry")

    private val Tasks = pencil :: milk :: apple :: banana :: orange :: grape :: strawberry :: Nil

    def getUser(userId: String): User = {
      Users.find(_.id == userId).get
    }

    def getTasksForUser(userId: String, limit: Int = 1, offset: Int = 0): (Int, List[Edge[Task]]) = {
      val totalCounts = Tasks.count { _.userId == userId }
      val edges: List[Edge[Task]] = Tasks.filter { _.userId == userId }.slice(offset, offset + limit).map {
        task => Edge(node = task, cursor = task.id)
      }

      (totalCounts, edges)
    }
  }

  val TaskType: ObjectType[Repo, Task] = ObjectType("Task", () => fields(
    Field("id", StringType, resolve = _.value.id),
    Field("userId", StringType, resolve = _.value.userId),
    Field("description", StringType, resolve = _.value.description)
  ))

  val customPageInfoType: ObjectType[Repo, CustomPageInfo] =
    ObjectType("CustomPageInfo",
      () => fields(
        Field("hasNextPage", BooleanType, Some("When paginating forwards, are there more items?"),
          resolve = _.value.hasNextPage),
        Field("hasPreviousPage", BooleanType, Some("When paginating backwards, are there more items?"),
          resolve = _.value.hasPreviousPage),
        Field("startCursor", OptionType(StringType), Some("When paginating backwards, the cursor to continue."),
          resolve = _.value.startCursor),
        Field("endCursor", OptionType(StringType), Some("When paginating forwards, the cursor to continue."),
          resolve = _.value.endCursor),
        Field("currentPage", IntType, Some("When paginating, where is the cursor?"),
          resolve = _.value.currentPage)
      )
    )

  val ConnectionDefinition(_, taskConnection) =
    Connection.definitionWithEdge[Repo, CustomPageInfo, CustomConnection, Task, Edge[Task]](
      name = "Task",
      nodeType = TaskType,
      pageInfoType = customPageInfoType
    )

  private object Args {
    val PageNo = Argument("pageNo", IntType)
    val PageSize = Argument("pageSize", IntType)
  }

  val UserType: ObjectType[Repo, User] = ObjectType("User", () => fields(
    Field("id", StringType, resolve = _.value.id),
    Field("name", StringType, resolve = _.value.name),
    Field("tasks", taskConnection, arguments = Args.PageNo :: Args.PageSize :: Nil, resolve = {
      ctx =>
        val (totalPage, currentPage, edges) = ctx.withArgs(Args.PageNo, Args.PageSize) {
          (pageNo, pageSize) =>
            val offset = (pageNo - 1) * pageSize
            val (totalCount, _edges) = ctx.ctx.getTasksForUser(ctx.value.id, pageSize, offset)
            val _totalPage = math.ceil(totalCount.toDouble / pageNo.toDouble)
            (_totalPage.toInt, pageNo, _edges)
        }

        CustomConnection[Task](
          CustomPageInfo(
            currentPage,
            hasNextPage = currentPage < totalPage,
            hasPreviousPage = 1 < currentPage
          ),
          edges
        )
    })
  ))

  val QueryType = ObjectType("Query", fields[Repo, Unit](
    Field("users", ListType(UserType), resolve = _.ctx.Users))
  )

  val schema = Schema(QueryType, additionalTypes = UserType :: TaskType :: Nil)

  "Connection.definitionWithEdge" should {
    "Includes pageInfo fields correctly" in {
      val Success(doc) = QueryParser.parse(
        """
          query UserQuery {
            users {
              id
              tasks(pageNo: 2, pageSize: 1) {
                pageInfo {
                  currentPage
                 }
                edges {
                  node {
                    id
                    description
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
            "users" -> List(
              Map(
                "id" -> "001",
                "tasks" ->
                  Map(
                    "pageInfo" -> Map(
                      "currentPage" -> 2
                    ),
                    "edges" -> List(
                      Map(
                        "node" ->
                          Map(
                            "id" -> "00b",
                            "description" -> "buy milk"
                          )
                      )
                    )
                  )
              ),
              Map(
                "id" -> "002",
                "tasks" ->
                  Map(
                    "pageInfo" -> Map(
                      "currentPage" -> 2
                    ),
                    "edges" -> List(
                      Map(
                        "node" ->
                          Map(
                            "id" -> "00d",
                            "description" -> "buy banana"
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

