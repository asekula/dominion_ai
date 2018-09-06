package dominion.src


class Player(val name: String, 
			 var player: ImmutablePlayer, 
			 val decisionMatrix: DecisionMatrix,
			 val valueFunction: (ImmutablePlayer => Double))