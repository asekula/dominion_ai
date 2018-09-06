package dominion.src

import scala.collection.immutable.HashMap

/**
  * This static object stores the value functions used in the ImmutablePlayer's best function. 
  * Note that these functions are computed by the resulting player after a string of decisions.
  * These are never computed on an immutable player that is not considering decisions. 
  * (unless it's being computed as an opponent)
  *
  * Note: The complexity of many of these functions is not great, as many compute expected value
  * of the player and/or it's opponents. In the final product, many of these will not be used.
  * These were mostly used for experimentation, but it is clear that as the decks get bigger,
  * many of these functions will be useless, since they would take a long time to compute. This
  * is why the genetic algorithm approach is used -- while it requires preprocessing, during
  * the game, computation will be quick.
  */
object Value {

	

	/**
	  * This function computes the player's value by taking the average cost of
	  * the cards that the player owns, whether on the field, in the hand, 
	  * in the discard, or in the deck. This is a really simple function, and
	  * using it in a player's .best() will result in the player buying the most
	  * expensive card, or performing actions that will maximize the average cost
	  * (which may include trashing low cost cards). (Note: the player may get stuck in
	  * a negative feedback loop of trashing cards, where trashing low cost cards is
	  * better than buying the best card it can. That's why this value function is not
	  * the best nor the most sophisticated.)
	  *
	  * @param player - the input player
	  * @return - the average cost of a card that the player owns
	  */
	def shallowValue(player: ImmutablePlayer): Double = {
		var value: Double = 0
		var total: Double = 0
		for ((card, count) <- player.allCards.cards) {
			value += (card.cost * count)
			total += count
		}
		if (total == 0) 0
		else value / total
	}

	// def simpleDecisions: DecisionMatrix = {

	// 	return new DecisionMatrix(HashMap[Card, ActionVector](
	// 			new Mine(None, None) -> action1,
	// 			new Laboratory -> action2,
	// 			new Witch -> action2,
	// 			new Village -> action1,
	// 			new Market -> action2,
	// 			new Moneylender -> action1,
	// 			new Chapel -> action2,
	// 			new Cellar -> action2,
	// 			new Remodel -> action1,
	// 			new Militia -> action1,
	// 			new Workshop -> action1,
	// 			new Smithy -> action2,
	// 			new Woodcutter -> action1,
	// 			new Festival -> action2,
	// 			new CouncilRoom -> action2,
	// 			new Moat -> action2,
	// 			new Feast -> action1,
	// 			new Chancellor -> action1,
	// 			new Bureaucrat -> action2,
	// 			new Copper -> action2,
	// 			new Silver -> action1,
	// 			new Gold -> action2,
	// 			new Curse -> action2,
	// 			new Estate -> action1,
	// 			new Duchy -> action2,
	// 			new Province -> action1
	// 	))
	// }

	// val action1 = new ActionVector(HashMap[Action, Double](
	// 						new Mine(None, None) -> 0.6,
	// 						new Laboratory -> 0.8,
	// 						new Witch -> 0.9,
	// 						new Village -> 0.8,
	// 						new Market -> 0.9,
	// 						new Moneylender -> 0.7,
	// 						new Chapel -> 0.5,
	// 						new Cellar -> 0.5,
	// 						new Remodel -> 0.5,
	// 						new Militia -> 0.6,
	// 						new Workshop -> 0.4,
	// 						new Smithy -> 0.5,
	// 						new Woodcutter -> 0.5,
	// 						new Festival -> 0.8,
	// 						new CouncilRoom -> 0.9,
	// 						new Moat -> 0.7,
	// 						new Feast -> 0.8,
	// 						new Chancellor -> 0.5,
	// 						new Bureaucrat -> 0.7
	// ))

	// val action2 = new ActionVector(HashMap[Action, Double](
	// 					new Mine(None, None) -> 0.3,
	// 					new Laboratory -> 0.5,
	// 					new Witch -> 0.7,
	// 					new Village -> 0.9,
	// 					new Market -> 0.1,
	// 					new Moneylender -> 0.1,
	// 					new Chapel -> 0.3,
	// 					new Cellar -> 0.8,
	// 					new Remodel -> 0.8,
	// 					new Militia -> 0.9,
	// 					new Workshop -> 0.1,
	// 					new Smithy -> 0.2,
	// 					new Woodcutter -> 0.4,
	// 					new Festival -> 0.3,
	// 					new CouncilRoom -> 0.9,
	// 					new Moat -> 0.2,
	// 					new Feast -> 0.2,
	// 					new Chancellor -> 0.8,
	// 					new Bureaucrat -> 0.9
	// ))

}