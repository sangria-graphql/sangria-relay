package sangria.relay.starWars

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.relay.starWars.StarWarsData.ShipRepo
import sangria.relay.util.AwaitSupport
import sangria.marshalling.InputUnmarshaller.mapVars

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class StarWarsMutationSpec extends WordSpec with Matchers with AwaitSupport {
  "Mutation" should {
    "Correctly mutates the data set" in {
      val Success(doc) = QueryParser.parse(
        """
          mutation AddBWingQuery($input: IntroduceShipInput!) {
            introduceShip(input: $input) {
              ship {
                id
                name
              }
              faction {
                name
              }
              clientMutationId
            }
          }
        """)

      val vars = mapVars(
        "input" -> Map(
          "shipName" -> "B-Wing",
          "factionId" -> "RmFjdGlvbjox",
          "clientMutationId" -> "abcde"
        )
      )
      
      Executor.execute(StarWarsSchema.schema, doc, variables = vars, userContext = new ShipRepo).await should be(
        Map(
          "data" -> Map(
            "introduceShip" -> Map(
              "ship" -> Map(
                "id" -> "U2hpcDo5",
                "name" -> "B-Wing"
              ),
              "faction" -> Map(
                "name" -> "Alliance to Restore the Republic"
              ),
              "clientMutationId" -> "abcde"
            ))))
    }
  }
}
