package sangria.relay.starWars

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.starWars.StarWarsData.ShipRepo
import sangria.relay.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class StarWarsObjectIdentificationSpec extends WordSpec with Matchers with AwaitSupport {
  "Object Identification" when {
    "Fetching" should {
      "Correctly fetches the ID and name of the rebels" in {
        val Success(doc) = QueryParser.parse(
          """
            query RebelsQuery {
              rebels {
                id
                name
              }
            }
          """)

        Executor.execute(StarWarsSchema.schema, doc, userContext = new ShipRepo).await should be(
          Map(
            "data" -> Map(
              "rebels" -> Map(
                "id" -> "RmFjdGlvbjox",
                "name" -> "Alliance to Restore the Republic"))))
      }

      "Correctly refetches the rebels" in {
        val Success(doc) = QueryParser.parse(
          """
            query RebelsRefetchQuery {
              node(id: "RmFjdGlvbjox") {
                id
                ... on Faction {
                  name
                }
              }
            }
          """)

        Executor.execute(StarWarsSchema.schema, doc, userContext = new ShipRepo).await should be(
          Map(
            "data" -> Map(
              "node" -> Map(
                "id" -> "RmFjdGlvbjox",
                "name" -> "Alliance to Restore the Republic"))))
      }

      "Correctly fetches the ID and name of the empire" in {
        val Success(doc) = QueryParser.parse(
          """
            query EmpireQuery {
              empire {
                id
                name
              }
            }
          """)

        Executor.execute(StarWarsSchema.schema, doc, userContext = new ShipRepo).await should be(
          Map(
            "data" -> Map(
              "empire" -> Map(
                "id" -> "RmFjdGlvbjoy",
                "name" -> "Galactic Empire"))))
      }

      "Correctly refetches the empire" in {
        val Success(doc) = QueryParser.parse(
          """
            query EmpireRefetchQuery {
              node(id: "RmFjdGlvbjoy") {
                id
                ... on Faction {
                  name
                }
              }
            }
          """)

        Executor.execute(StarWarsSchema.schema, doc, userContext = new ShipRepo).await should be(
          Map(
            "data" -> Map(
              "node" -> Map(
                "id" -> "RmFjdGlvbjoy",
                "name" -> "Galactic Empire"))))
      }

      "Correctly refetches the X-Wing" in {
        val Success(doc) = QueryParser.parse(
          """
            query XWingRefetchQuery {
              node(id: "U2hpcDox") {
                id
                ... on Ship {
                  name
                }
              }
            }
          """)

        Executor.execute(StarWarsSchema.schema, doc, userContext = new ShipRepo).await should be(
          Map(
            "data" -> Map(
              "node" -> Map(
                "id" -> "U2hpcDox",
                "name" -> "X-Wing"))))
      }
    }
  }
}
