package dominion.src

import scala.collection.mutable.ListBuffer

// Note: Action is the action that was already made.
abstract class AbsDecision(val action: Action, val player: ImmutablePlayer) {
	def best(decisionMatrix: DecisionMatrix, 
			 valueFunction: (ImmutablePlayer => Double)): (ImmutablePlayer, List[Action]) 
}


// Important: ContinueDecisions can never be the end of the decision train! Either HaltDecision or BuyCardDecision can be.
class ContinueDecision(override val action: Action, override val player: ImmutablePlayer) extends AbsDecision(action, player) {

	def this(player: ImmutablePlayer) {
		this(new NoAction, player)
	}

	def best(decisionMatrix: DecisionMatrix, 
			 valueFunction: (ImmutablePlayer => Double)): (ImmutablePlayer, List[Action]) = {
		
		val decisions = bestNextDecisions(decisionMatrix, player) // Could be PlayTreasures.
		
		val nextDecision = bestDecision(decisions, valueFunction)
		val nextBest = nextDecision.best(decisionMatrix, valueFunction)
		return (nextBest._1, action :: nextBest._2)
	}

	// Todo: Clean up this code.
	// Note: Only takes an action that can make a decision (will reject Mine if there are no Coppers or Silvers in the hand)
	// Important: matrix.multiply(CardSet) treats all positive integers as 1, as does Vector's .dot.
	private def bestNextDecisions(matrix: DecisionMatrix, player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		if (player.actions > 0) { // Saves matrix multiplication if == 0.
			var vector: ActionVector = matrix.multiply(player.hand).dot(player.hand)
			
			while (vector.actionCards.size > 0) {
				val bestActionTuple: (Action, Double) = vector.max
				if (bestActionTuple._2 != 0) {
					val decisions = bestActionTuple._1.decisions(player)
					if (decisions.size > 0) return decisions
					else vector = vector.remove(bestActionTuple._1)
				} else {
					vector = vector.remove(bestActionTuple._1)
				}
			}
		}
		
		return (new PlayTreasuresAction).decisions(player)
	}

	private def bestDecision(decisions: ListBuffer[AbsDecision], valueFunction: (ImmutablePlayer => Double)): AbsDecision = {
		if (decisions.size == 0) println("Error player: " + player)

		//for (dec <- decisions) {
			//if (dec.isInstanceOf[BuyCardDecision]) {
				//println("Buying: " + dec.action.asInstanceOf[PlayTreasuresAction].buy)
				//println("Value of: " + valueFunction(dec.player))
				//println("\n\n")
		//	}
		//}

		// Weird hack for cellar, I don't even know if I like it...
		// ^solves the cellar problem for now, at least the decisions are valued differently.
		// Note: (If you don't understand d.player.withoutDiscard) if the decision is player,
		// the AI will discard the least-valued cards in the game, because here it thinks it's
		// trashing the cards. (i.e. withoutDiscard treats discarding the cards as trashing.
		// since the values are relative, the only difference in value will be caused by 
		// discarding certain cards in the hand.)
		if (decisions.head.action.isInstanceOf[Cellar]) {
			return decisions.maxBy(d => valueFunction(d.player.withoutDiscard))
		} else {
			return decisions.maxBy(d => valueFunction(d.player))
		}
	}
}
