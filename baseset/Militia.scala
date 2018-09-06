package dominion.src

import scala.math
import scala.collection.mutable.ListBuffer


case class Militia() extends ActionAttack(4, 0, 0, 0, 2) {

	/**
	  * Generates the possible opponentActions that player can make when militia is being
	  * played against the player. First discards victory cards, then considers all other 
	  * combinations of cards to discard.
	  *
	  * @param player - the player being attacked
	  * @return - the possible actions the player can take in reaction to the attack.
	  */
	def opponentActions(player: ImmutablePlayer): ListBuffer[OpponentAction] = {

		if (player.hand.cards.contains(new Moat)) return ListBuffer(new NoOpponentAction)
		else {
			val next = new ListBuffer[OpponentAction]
			val victoryCards = player.hand.cards.filter(c => c._1.isInstanceOf[VictoryCard])
			val totalVC = victoryCards.map(vc => vc._2).sum
			val needToRemove = math.max(player.hand.size - 3, 0)
			val vcsToRemove = math.min(totalVC, needToRemove)
			
			val anyCombination = HandGenerator.anyCombinationOfNumber(new CardSet(victoryCards), vcsToRemove)
			val nextPlayer = player.discard(anyCombination)
			
			for (comb <- HandGenerator.combinationsOfNumber(nextPlayer.hand, math.max(needToRemove - vcsToRemove, 0))) {
				next += new Discard(anyCombination.addAll(comb))
			}

			return next
		}
	}
}