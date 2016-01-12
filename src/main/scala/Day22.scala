import scala.collection.mutable.ListBuffer

/**
  * Created by robert on 6-1-16.
  */
object Day22 {
  var debug = false

  class Spell(val name: String, val cost: Int, val result: (Player, Player, Player) => Boolean) {
    override def toString(): String = name
  }

  class NormalSpell(name: String, cost: Int, result: (Player, Player, Player) => Boolean) extends Spell(name, cost, result)
  class Effect(name: String, cost: Int, val timer: Int, val init: (Player, Player, Player) => Boolean, val stop: (Player, Player) => Boolean = { (_, _) => true }) extends Spell(name, cost, init) {
    def used(): Effect = new Effect( name, cost, timer - 1, init, stop)
  }

  abstract class Player(val name: String, var hitpoints: Int, var damage: Int = 0, var armor: Int = 0, var mana: Int = 0) {
    override def toString(): String = name + ": " + hitpoints + " HP / " + damage + " Damage / " + mana + " Mana"

    var effects = List.empty[Effect]

    def initEffects(opponent: Player, playing: Boolean = true) = {
      // First perform all active effects
      effects = effects.map((effect) => {
        effect.init(this, opponent, if(playing) this else opponent )
        val newEffect = effect.used()

        if (debug) println("  Doing effect for " + effect + "; timer is now " + newEffect.timer)
        newEffect
      })
    }

    def deinitEffects(opponent: Player, playing: Boolean = true) = {
      // Deinitialize the effects
      effects.foreach((effect) => {
        effect.stop(this, opponent)
      })

      // Remove effects not effective anymore
      effects = effects.filter(_.timer > 0)
    }

    def play(opponent: Player, spell: Spell): Boolean

    def hurt(amount: Int): Boolean = {
      val amountDamage = Math.max(1, amount - armor)

      if (hitpoints < amountDamage) {
        hitpoints = 0
        return false
      } else {
        hitpoints = hitpoints - amountDamage
        return true
      }
    }

    def heal(amount: Int): Boolean = {
      hitpoints = hitpoints + amount
      true
    }

    def updateMana(amount: Int): Boolean = {
      mana = mana + amount
      true
    }

    def updateArmor(amount: Int): Boolean = {
      armor = Math.max(0, armor + amount)
      true
    }

    def finished(): Boolean = hitpoints <= 0
  }

  class Wizard(_name: String, _hitpoints: Int, _mana: Int = 0) extends Player(_name, _hitpoints, 0, 0, _mana) {
    override def toString(): String = name + ": " + hitpoints + " HP / " + mana + " Mana"

    override def play(opponent: Player, spell: Spell): Boolean = {
      initEffects(opponent)

      if (finished || opponent.finished)
        return true

      // Deduct the mana
      if (spell.cost > mana)
        throw new Exception("Cannot cast spell " + spell.name + ": not enough mana: " + mana)

      // Can't cast a spell that is still active (however, a spell can be cast
      // in the same turn an existing
      if (spell.isInstanceOf[Effect] && effects.find((effect) => effect.name == spell.name && effect.timer > 0).isDefined)
        throw new Exception("Cannot cast spell " + spell.name + ": there is already such an active spell")

      if (debug) println("  " + name + " casts spell " + spell)

      updateMana(-spell.cost)

      // Cast the spell itself
      spell match {
        case normalSpell: NormalSpell => normalSpell.result(this, opponent, this)
        case effect: Effect => effects = effects :+ effect
      }

      deinitEffects(opponent)

      return (finished || opponent.finished)
    }
  }

  class Boss(_name: String, _hitpoints: Int, _damage: Int) extends Player(_name, _hitpoints, _damage) {
    override def toString(): String = name + ": " + hitpoints + " HP"

    override def play(opponent: Player, spell: Spell = null): Boolean = {
      opponent.initEffects(this, false)

      if (finished || opponent.finished)
        return true

      // Hurt the opponent
      opponent.hurt(damage)

      if (debug) println("  " + name + " does " + damage + " damage to " + opponent.name)

      opponent.deinitEffects(this, false)

      return (finished || opponent.finished)
    }

  }

  def turn(player: Player, opponent: Boss, spell: Spell) = {
    if (debug) println("Player turn  " + player + " plays " + spell)
    player.play(opponent, spell)

    if (debug) println("  (result: " + player + " / " + opponent + ")")
    if (player.hitpoints > 0 && opponent.hitpoints > 0) {
      if (debug) println("Opponent turn " + opponent)
      opponent.play(player)
      if (debug) println("  (result: " + player + " / " + opponent + ")")
    }
  }

  def play(player: Player, opponent: Boss, spells: List[Spell]): Boolean = {
    var attacker = player
    var defender = opponent
    var spellsLeft = spells

    while (spellsLeft.size > 0 && player.hitpoints > 0 && opponent.hitpoints > 0) {
      turn(player, opponent, spellsLeft.head)
      spellsLeft = spellsLeft.tail
    }

    true
  }

  /**
    * Calculates all permutations taking n elements of the input List,
    * with repetitions.
    * Precondition: input.length > 0 && n > 0
    */
  def permutationsWithRepetitions[T](input: List[T], n: Int): List[List[T]] = {
    require(input.length > 0 && n > 0)
    n match {
      case 1 => for (el <- input) yield List(el)
      case _ => for (el <- input; perm <- permutationsWithRepetitions(input, n - 1)) yield el :: perm
    }
  }

  def spells(): List[Spell] = {
    List(
      new NormalSpell("Magic Missile", 53, (player: Player, opponent: Player, _) => {
        opponent.hurt(4)
      }),
      new NormalSpell("Drain", 73, (player: Player, opponent: Player, _) => {
        player.heal(2) && opponent.hurt(2)
      }),
      new Effect("Shield", 113, 6, (player: Player, opponent: Player, _) => {
        player.updateArmor(7)
      }, (player: Player, opponent: Player) => {
        player.updateArmor(-7)
      }),
      new Effect("Poison", 173, 6, (player: Player, opponent: Player, _) => {
        opponent.hurt(3)
      }),
      new Effect("Recharge", 229, 5, (player: Player, opponent: Player, _) => {
        player.updateMana(101)
      })
    )
  }

  def hardModeEffect(): Effect = new Effect("HardMode", 0, Int.MaxValue, (player: Player, opponent: Player, whoseTurn: Player) => {
    if( whoseTurn == player )
      player.hurt(1)

    true
  })

  def actual(args: Array[String]) {
    val hardMode = (args.size > 0 && args(0) == "hard")
    val availableSpells = spells()

    // Start looking for the next cheapest solution
    var depth = 1
    val maxDepth = 30
    var currentBest = Int.MaxValue

    val validPrefixes: scala.collection.mutable.ListBuffer[List[Spell]] = ListBuffer(List())

    while (depth < maxDepth && validPrefixes.size > 0) {
      val combinationsToTry = for (prefix <- validPrefixes; spell <- availableSpells) yield prefix :+ spell

      println("Depth " + depth)
      println("Valid prefixes: " + validPrefixes.size)
      println("Combinations to try: " + combinationsToTry.size + " / " + Math.pow(5, depth).toInt)

      validPrefixes.clear()

      for (combi <- combinationsToTry) {
        // Only try this combination if it is better than the current solution
        val currentCost = combi.map(_.cost).sum
        if (currentCost < currentBest) {
          //          val opponent = new Boss("Boss", 58, 9)
          //          val player = new Player("Player", 50, 0, 0, 500)
          val opponent = new Boss("Boss", 58, 9)
          val player = new Wizard("Player", 50, 500)

          if( hardMode ) {
            player.effects = player.effects :+ hardModeEffect
          }

          try {
            play(player, opponent, combi)

            // If the opponent died and the player didn't, we have a proper result
            if (opponent.finished && !player.finished) {
              println(combi)
              println("Found better solution: " + currentCost)
              currentBest = currentCost
            } else if (!opponent.finished && !player.finished) {
              // If both players are still alive, we should try to cast other spells
              if (debug) println("Undetermined")
              validPrefixes += combi
            } else {
              if(debug) println("No solution: " + opponent.hitpoints + " / " + player.hitpoints)
            }
          } catch {
            // The combination is invalid for some reason, this list is invalid
            case e: Exception => {
              if(debug) println(e.getMessage + " / " + combi)
            }
          }
        }
      }

      depth = depth + 1
    }


    println("----------------------")
    println("Best solution: " + currentBest)

  }

  def test(args: Array[String]) {
    val hardMode = (args.size > 0 && args(0) == "hard")

    debug = true
    val availableSpells = spells()

    val combi = List(
      availableSpells(4),
      availableSpells(3),
      availableSpells(1),
      availableSpells(0),
      availableSpells(2)
    )
    val opponent = new Boss("Boss", 60, 8)
    val player = new Wizard("Player", 60, 1000)

    if( hardMode ) {
      player.effects = player.effects :+ hardModeEffect
    }

    println("Playing combination " + combi.mkString(", "))

    println(play(player, opponent, combi))
    println("--- End ---")
    println(player)
    println(opponent)
  }


  def main(args: Array[String]) {
    val runType = if (args.size > 0) args(0) else "actual"

    runType match {
      case "test" => test(args.tail)
      case _ => actual(args.tail)
    }
  }
}