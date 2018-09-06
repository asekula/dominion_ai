package dominion.src

import scala.collection.mutable.ListBuffer


abstract class ActionAttack(override val cost: Int, 
							override val actions: Int, 
							override val buys: Int, 
							override val cards: Int, 
							override val coin: Int) extends Action(cost, actions, buys, cards, coin) {	

	def opponentActions(player: ImmutablePlayer): ListBuffer[OpponentAction]

	// Used in API
	//def decisionAsOpponent(player: ImmutablePlayer): OpponentDecision = new OpponentContinueDecision(player, this)

	// Used in Game
	//def invokeAsOpponent(player: ImmutablePlayer, valueFunction: (ImmutablePlayer => Double)): ImmutablePlayer = {
	//	return decisionAsOpponent(player).best(valueFunction)._1
	//}
}