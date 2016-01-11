/**
  * Created by robert on 6-1-16.
  */
object Day21 {
  case class Item(val name: String, val cost: Int, val damage: Int, val armor: Int)
  class Player(val name: String, var hitpoints: Int, val damage: Int, val armor: Int, val cost: Int = 0) {
    override def toString(): String = name + ": " + hitpoints + " / " + damage + " / " + armor + " (" + cost + " gold)"
  }

  def itemShop(filename: String = "/day21/weapons.txt"): Map[String,List[Item]] = {
    val items = scala.collection.mutable.HashMap.empty[String,List[Item]]
    var currentCategory: String = null

    for( line <- Utils.getStream(filename).getLines ) {
      if( line.trim().length > 0 ) {
        val parts = line.split(" +")

        // Special case:
        if (parts(0).endsWith(":")) {
          currentCategory = parts(0).replace(":", "")
          items(currentCategory) = List.empty
        } else {
          items(currentCategory) = items(currentCategory) :+ new Item(parts(0), parts(1).toInt, parts(2).toInt, parts(3).toInt)
        }
      }
    }

    items.toMap
  }

  def initializePlayer(items: List[Item]): Player = {
    new Player( "Player", 100, items.map( _.damage ).sum, items.map( _.armor ).sum, items.map( _.cost ).sum )
  }

  def generatePlayers(shop: Map[String,List[Item]]): List[Player] = {
    // You must buy exactly one weapon; no dual-wielding. Armor is optional,
    // but you can't use more than one. You can buy 0-2 rings (at most one for each hand).
    // You must use any items you buy. The shop only has one of each item, so you can't buy,
    // for example, two rings of Damage +3.
    for(
      weapon <- shop("Weapons");
      armor <- shop("Armor");
      rings <- (shop("Rings").combinations(2) ++ shop("Rings").combinations(1) ++ Iterator(List()) ) )
      yield initializePlayer( rings :+ armor :+ weapon )
  }

  def turn( attacker: Player, defender: Player ) = {
    defender.hitpoints = defender.hitpoints - Math.max(1, attacker.damage - defender.armor )
  }

  def play( player: Player, opponent: Player ): Boolean = {
    var attacker = player
    var defender = opponent

    while( player.hitpoints > 0 && opponent.hitpoints > 0 ) {
      turn( attacker, defender )

      // Swap attacker and defender
      val tmp = attacker
      attacker = defender
      defender = tmp
    }

    return player.hitpoints > opponent.hitpoints
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day21/weapons.txt"
    val opponent = new Player( "Opponent", 100, 8, 2)

    println("Generating players")
    val players = generatePlayers(itemShop(filename))

    println( "# players: " + players.size )

    var success = false
    for( player <- players.sortWith( _.cost > _.cost ) ) {
      if( !success ) {
        opponent.hitpoints = 100

        if (!play(player, opponent)) {
          println("Final player: " + player)
          success = true
        }
      }
    }
  }

}
