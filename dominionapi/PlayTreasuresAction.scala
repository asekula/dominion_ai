package dominion.src

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.HashMap
import scala.math

case class PlayTreasuresAction(buy: Option[CardSet]) extends Action(0) {
	
	def this() { this(None) }
	
	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val next = new ListBuffer[AbsDecision]()
		val totalValue = player.handValue
		val supplyWithoutCopperOrCurse = player.supply.removeKey(new Curse).removeKey(new Copper) // doesn't allow player to buy copper or curse (saves computation)
		val allPossibleBuys = HandGenerator.allPossibleBuys(supplyWithoutCopperOrCurse, totalValue, math.min(6, player.buys)) // limits to 6 buys.
		
		for (possibleBuy <- allPossibleBuys) { // possibleBuy is a CardSet
			val buyAction = new PlayTreasuresAction(Some(possibleBuy))
			next += new BuyCardDecision(buyAction, player.play(buyAction))
		}
		
		return next
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = buy match {
		case Some(cards) => new ImmutablePlayer(
										p.supply.removeAll(cards),
										p.discard.addAll(cards),
										p.field,
										p.hand,
										p.deck,
										0,
										0, // Exhaust buys.
										0,
										p.topOfDeck,
										0) // If we're buying then there should not have been any draw cards.
		case None => p
	}

	override def toString: String = buy match {
		case Some(cards) => "PlayTreasures(buy: " + cards + ")"
		case None => "PlayTreasures"
	}

	override def toEnglish: String = buy match {
		case Some(cards) =>
			if (cards.size > 0) "Play Treasures, and buy " + cards.toEnglish + "."
			else "End round."
		case None => "Play Treasures"
	}
}
