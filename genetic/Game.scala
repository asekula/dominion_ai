package dominion.src

import scala.collection.immutable.Set
import scala.collection.immutable.HashMap

class Game(decision1: DecisionMatrix, value1: (ImmutablePlayer => Double),
		   decision2: DecisionMatrix, value2: (ImmutablePlayer => Double)) {
	

	def this(org1: Organism, org2: Organism) {
		this(org1.decisionMatrix, org1.valueDistribution.asValueFunction,
		     org2.decisionMatrix, org2.valueDistribution.asValueFunction)
	}


	val supply = new CardSet(HashMap[Card, Int]( // Ordered by price.
						new Province -> 12,
						new Gold -> 50,
						new Silver -> 50,
						new Copper -> 50,
						new Curse -> 15,
						new Mine(None, None) -> 10,
						new Laboratory -> 10,
						new Witch -> 10,
						new Village -> 10,
						new Market -> 10,
						new Moneylender -> 10,
						new Chapel -> 10,
						new Cellar -> 10,
						new Remodel -> 10,
						new Militia -> 10,
						new Workshop -> 10,
						new Smithy -> 10,
						new Woodcutter -> 10,
						new Festival -> 10))

	val deck = new CardSet(HashMap[Card, Int](
				new Copper -> 7,
				new Estate -> 3))
	

	val player1 = new ImmutablePlayer(supply, new CardSet, new CardSet, new CardSet, deck, 1, 1, 0, List(), 5)
	val player2 = new ImmutablePlayer(supply, new CardSet, new CardSet, new CardSet, deck, 1, 1, 0, List(), 5)

	var current = new Player("p1", player1.draw, decision1, value1)
	var opponent = new Player("p2", player2.draw, decision2, value2)
	var startingSupply = player1.supply

	val DEBUG = false
	val SHOW_RESULTS = false

	private def playRound {

		if (DEBUG) {
			println("************************Starting Round************************")
			println("\t\tCurrent: " + current.player)
			println("\t\tOpponent: " + opponent.player)
			println("************************************************************")
		}
		val dec = new ContinueDecision(current.player)
		val best = dec.best(current.decisionMatrix, current.valueFunction)
		if (DEBUG) println("Current player actions: " + best._2.tail)
		current.player = best._1

		val playedTreasures: Boolean = best._2.filter(a => a.isInstanceOf[PlayTreasuresAction]).size > 0

		// Since the supply is shared.
		opponent.player = opponent.player.setSupply(current.player.supply)

		for (action <- best._2) {
			if (action.isInstanceOf[ActionAttack]) {
				val actionAttack = action.asInstanceOf[ActionAttack]
				val oppDec = new OpponentDecision(actionAttack, opponent.player)
				val bestOpp = oppDec.best(opponent.decisionMatrix, opponent.valueFunction)
				if (DEBUG) println("Opponent responds: " + bestOpp._2)
				opponent.player = bestOpp._1

				if (opponent.player.needsToDraw > 0) {
					if (DEBUG) println("Opponent draws: " + opponent.player.needsToDraw)
					opponent.player = opponent.player.draw
				}
			}
		}

		current.player = current.player.setSupply(opponent.player.supply)

		if (current.player.needsToDraw > 0) {
			if (DEBUG) println("Current draws: " + current.player.needsToDraw)
			current.player = current.player.draw
		}

		if (current.player.actions == 0 && playedTreasures) {
			current.player = current.player.nextRound.draw
			val temp = current // Todo: Make sure this works.
			current = opponent
			opponent = temp
			if (DEBUG) println("Players switch.")
		} else {
			if (DEBUG) println("Current player continues.")
		}
	}

	def playGame: Boolean = {

		var round = 0
		val maxRounds = 100

		// Important: player1.draw <- draws first hand.
		if (randomBool) {
			current = new Player("p1", player1.draw, decision1, value1)
			opponent = new Player("p2", player2.draw, decision2, value2)
		} else {
			current = new Player("p2", player2.draw, decision2, value2)
			opponent = new Player("p1", player1.draw, decision1, value1)
		}

		startingSupply = player1.supply

		while (!current.player.gameOver(startingSupply) && round < maxRounds) {
			round += 1
			playRound
		}

		if (SHOW_RESULTS) {
			println("Final Results:")
			println(current.name + ": " + current.player.totalPoints)
			println(opponent.name + ": " + opponent.player.totalPoints)
		}

		if (current.name == "p1") return current.player.totalPoints > opponent.player.totalPoints
		else return opponent.player.totalPoints >= current.player.totalPoints
	}

	def playTournament(games: Int): Double = {
		var all = 0
		var p1Wins = 0
		for (a <- 1 to games) {
			if (playGame) p1Wins += 1
			all += 1 // Don't really need this.
			//loading(a, games)
		}

		//println("\nP1 Wins: " + (p1Wins.toDouble / all.toDouble))
		return (p1Wins.toDouble / all.toDouble) // Returns win percentage.
	}

	def randomBool: Boolean = math.random < 0.5

	def loading(counter: Int, total: Int) {
		if (total < 50) return

		if (counter % (total/10) == 0) {
			print(counter / (total/10))
		} else if (counter % (total / 50) == 0) {
			print("#")
		}
	}
}

object Game {
	def main(args: Array[String]) {
		println("It has begun.")

		val game = new Game(DecisionMatrix.randomMatrix, Value.shallowValue, DecisionMatrix.randomMatrix, Value.shallowValue)
		game.playTournament(100)
	}
}
