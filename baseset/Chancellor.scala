package dominion.src


import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class Chancellor(deckToDiscard: Option[Boolean]) extends Action(3) {

	def this() { this(None) }

	override def clear: Card = new Chancellor()


	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val noChancellor = new Chancellor(Some(false))
		val yesChancellor = new Chancellor(Some(true))
		val deckVal = player.avgCostNonVictory(player.deck)
		val deckAndDiscardVal = player.avgCostNonVictory(player.deck.addAll(player.discard))
		
		if (deckVal >= deckAndDiscardVal) { // Computes avg cost, but treats victory cards' cost as 0. 
			ListBuffer[AbsDecision](new ContinueDecision(noChancellor, player.play(noChancellor)))
		}
		else ListBuffer[AbsDecision](new ContinueDecision(yesChancellor, player.play(yesChancellor)))
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = deckToDiscard match {
		case None => p
		case Some(d) => 
			if (d) {
				new ImmutablePlayer(
								p.supply, 
								p.discard.addAll(p.deck), 
								p.field.addOne(this), 
								p.hand.removeOne(new Chancellor(None)),
								new CardSet,
								p.actions - 1,
								p.buys, 
								p.coin + 2,
								p.topOfDeck,
								0)

			} else {
				new ImmutablePlayer(
								p.supply, 
								p.discard, 
								p.field.addOne(this),
								p.hand.removeOne(new Chancellor(None)),
								p.deck,
								p.actions - 1,
								p.buys, 
								p.coin + 2,
								p.topOfDeck,
								0)
			}
	}

	override def toString: String = deckToDiscard match {
		case None => "Chancellor"
		case Some(d) => "Chancellor(discard deck: " + d + ")"
	}			


	override def toEnglish: String = deckToDiscard match {
		case None => "Chancellor"
		case Some(d) => 
			if (d) "Chancellor: Discard your deck."
			else "Chancellor: Do not discard your deck."
	}	
}