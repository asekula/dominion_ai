package dominion.src


class BuyCardDecision(override val action: PlayTreasuresAction, 
					  override val player: ImmutablePlayer) extends AbsDecision(action, player) {
	
	def best(decisionMatrix: DecisionMatrix, 
		 valueFunction: (ImmutablePlayer => Double)): (ImmutablePlayer, List[Action]) = 
		(player, List[Action](action))
}