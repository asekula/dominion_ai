package dominion.src

import scala.collection.mutable.ListBuffer

/** 
  * Sometimes the player may need to make decisions while the other players are playing,
  * like discarding a card, or drawing a card, and those decisions are represented here.
  * Note that these decisions are made by the current player when the opponents are playing.
  * 
  * This is done one action attack at a time, since action is required immediately after an action
  * attack is played. (You can't hold off discarding down to three until you know someone will play a Council Room after 
  * someone played a Militia. You need to discard down to 3, not knowing whether someone will play a Council Room
  * after that.)
  *
  * @param actionAgainst - the action attack taken against the player. (this may be council room, which is 
  *							treated as an action attack.)
  */
class OpponentDecision(val actionAgainst: ActionAttack,
							   override val player: ImmutablePlayer) extends AbsDecision(actionAgainst, player) {
	
	private val actionOptions: ListBuffer[OpponentAction] = actionAgainst.opponentActions(player)

	def best(decisionMatrix: DecisionMatrix,
		     valueFunction: (ImmutablePlayer => Double)): (ImmutablePlayer, List[OpponentAction]) = {
		val bestAction = actionOptions.maxBy(a => resultOfAction(a.invoke(player), decisionMatrix))
		return (bestAction.invoke(player), List(bestAction))
	}

	private def resultOfAction(player: ImmutablePlayer, decisionMatrix: DecisionMatrix): Double = {
		return decisionMatrix.multiply(player.hand).sum // Q: Is this correct?
	}
}

/**
  * Represents drawing a card as an opponent.
  *
  * @param draw - the number of cards to draw
 
class OpponentHaltDecision(val actionAgainst: ActionAttack,
						   override val player: ImmutablePlayer, 
						   val draw: Int) extends AbsDecision(actionAgainst, player) {
	
	def best(decisionMatrix: DecisionMatrix,
		     valueFunction: (ImmutablePlayer => Double)): (ImmutablePlayer, List[OpponentAction]) = {
		(player, List(new DrawCards(draw)))
	}
}*/