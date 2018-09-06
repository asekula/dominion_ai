package dominion.src


class HaltDecision(override val action: Action, 
					override val player: ImmutablePlayer) extends AbsDecision(action, player) {
	
	def best(decisionMatrix: DecisionMatrix, 
			 valueFunction: (ImmutablePlayer => Double)): (ImmutablePlayer, List[Action]) = 
		(player, List(action))
}
