package dominion.src

import scala.collection.mutable.ListBuffer

// Cost, Actions, Buys, Cards, Coin
case class Village() extends Action(3, 2, 0, 1, 0)
case class Festival() extends Action(5, 2, 1, 0, 2)
case class Laboratory() extends Action(5, 1, 0, 2, 0)
case class Market() extends Action(5, 1, 1, 1, 1)
case class Smithy() extends Action(4, 0, 0, 3, 0)
case class Moat() extends Action(2, 0, 0, 2, 0)
case class Woodcutter() extends Action(3, 0, 1, 0, 2)

// Still make an OpponentChanceDecision even if there is only one. We still need to get the value from best.
case class CouncilRoom() extends ActionAttack(5, 0, 1, 4, 0) { // ActionAttack because it affects the other player.
	
	def opponentActions(player: ImmutablePlayer): ListBuffer[OpponentAction] = 
		ListBuffer(new DrawCards(1))

	override def plain: String = "Council Room"
	override def toEnglish: String = "Council Room"

	// Overridden because this is the only action attack that resulted in an OpponentHaltDecision.
	//override def decisionAsOpponent(player: ImmutablePlayer): OpponentDecision = 
	//	new OpponentHaltDecision(player, this, 1)
}

case class Witch() extends ActionAttack(5, 0, 0, 2, 0) {

	def opponentActions(player: ImmutablePlayer): ListBuffer[OpponentAction] = {
		if (player.hand.cards.contains(new Moat) || !player.supply.cards.contains(new Curse)) ListBuffer(new NoOpponentAction)
		else ListBuffer(new AddToDiscard(new Curse))
	}
}