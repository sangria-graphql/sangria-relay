package sangria.relay

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.util.AwaitSupport
import sangria.schema._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class NodePluralSpec extends WordSpec with Matchers with AwaitSupport {
  case class User(userName: String, url: String)

  val UserType = ObjectType("User",
    fields[Unit, User](
      Field("userName", OptionType(StringType), resolve = _.value.userName),
      Field("url", OptionType(StringType), resolve = _.value.url)))

  val QueryType: ObjectType[Unit, Unit] = ObjectType("Query",
    fields[Unit, Unit](
      Node.pluralIdentifyingRootField(
        fieldName = "userNames",
        description = Some("Map from a username to the user"),
        fieldType = OptionType(UserType),
        argName = "userNames",
        argType = StringType,
        resolveSingleInput = (userName: String, ctx: Context[Unit, Unit]) =>
          Some(User(userName, "www.facebook.com/" + userName))
      )
    ))

  val schema = Schema(QueryType)

  "pluralIdentifyingRootField" should {
    "Allows fetching" in {
      val Success(doc) = QueryParser.parse(
        """
          {
            userNames(userNames: ["dschafer", "leebyron", "schrockn"]) {
              userName
              url
            }
          }
        """)


      Executor.execute(schema, doc).await should be  (
        Map(
          "data" -> Map(
            "userNames" -> List(
              Map(
                "userName" -> "dschafer",
                "url" -> "www.facebook.com/dschafer"
              ),
              Map(
                "userName" -> "leebyron",
                "url" -> "www.facebook.com/leebyron"
              ),
              Map(
                "userName" -> "schrockn",
                "url" -> "www.facebook.com/schrockn"
              )
            ))))
    }

    "Correctly introspects" in {
      val Success(doc) = QueryParser.parse(
        """
          {
            __schema {
              queryType {
                fields {
                  name
                  args {
                    name
                    type {
                      kind
                      ofType {
                        kind
                        ofType {
                          kind
                          ofType {
                            name
                            kind
                          }
                        }
                      }
                    }
                  }
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
        """)


      Executor.execute(schema, doc).await should be  (
        Map(
          "data" -> Map(
            "__schema" -> Map(
              "queryType" -> Map(
                "fields" -> List(
                  Map(
                    "name" -> "userNames",
                    "args" -> List(
                      Map(
                        "name" -> "userNames",
                        "type" -> Map(
                          "kind" -> "NON_NULL",
                          "ofType" -> Map(
                            "kind" -> "LIST",
                            "ofType" -> Map(
                              "kind" -> "NON_NULL",
                              "ofType" -> Map(
                                "name" -> "String",
                                "kind" -> "SCALAR"
                              )
                            )
                          )
                        )
                      )
                    ),
                    "type" -> Map(
                      "kind" -> "LIST",
                      "ofType" -> Map(
                        "name" -> "User",
                        "kind" -> "OBJECT"
                      )
                    )
                  )
                )
              )
            ))))
    }
  }
}