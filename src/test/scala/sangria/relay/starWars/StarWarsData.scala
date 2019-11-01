package sangria.relay.starWars

import java.util.concurrent.atomic.AtomicInteger

import sangria.relay.{Identifiable, Node}

/**
 * This defines a basic set of data for our Star Wars Schema.
 *
 * This data is hard coded for the sake of the demo, but you could imagine
 * fetching this data from a backend service rather than from hardcoded
 * JSON objects in a more complex demo.
 */
object StarWarsData {
  case class Ship(id: String, name: String) extends Node
  case class Faction(id: String, name: String, ships: List[String])

  object Faction {
    implicit object FactionIdentifiable extends Identifiable[Faction] {
      def id(faction: Faction) = faction.id
    }
  }

  object Ships {
    val xwing = Ship("1", "X-Wing")
    val ywing = Ship("2", "Y-Wing")
    val awing = Ship("3", "A-Wing")
    val falcon = Ship("4", "Millenium Falcon")
    val homeOne = Ship("5", "Home One")
    val tieFighter = Ship("6", "TIE Fighter")
    val tieInterceptor = Ship("7", "TIE Interceptor")
    val executor = Ship("8", "Executor")
    
    val All = xwing :: ywing :: awing :: falcon :: homeOne :: tieFighter :: tieInterceptor :: executor :: Nil 
  }

  object Factions {
    val rebels = Faction("1", "Alliance to Restore the Republic", List("1", "2", "3", "4", "5"))
    val empire = Faction("2", "Galactic Empire", List("6", "7", "8"))

    val All = rebels :: empire :: Nil
  }

  class ShipRepo {
    val nextShipId = new AtomicInteger(9)

    var ships = Ships.All
    var factions = Factions.All

    def createShip(shipName: String, factionId: String) = {
      val newShip = Ship("" + nextShipId.getAndIncrement(), shipName)

      ships = ships :+ newShip
      factions = factions.map {
        case f if f.id == factionId => f.copy(ships = f.ships :+ newShip.id)
        case f => f
      }

      newShip
    }

    def getShip(id: String) = ships find (_.id == id)
    def getFaction(id: String) = factions find (_.id == id)
    def getRebels = factions find (_.id == "1")
    def getEmpire = factions find (_.id == "2")
  }
}
