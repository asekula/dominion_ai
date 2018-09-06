package dominion.src

import scala.collection.mutable.ListBuffer

abstract class Action(override val cost: Int, val actions: Int, val buys: Int, val cards: Int, val coin: Int) extends Card(cost) {
	
	def this(cost: Int) {
		this(cost, 0, 0, 0, 0)
	}

	// To be overriden by more complicated action cards.
	def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		if (cards == 0) {
			ListBuffer[AbsDecision](new ContinueDecision(this, player.play(this)))
		} else {
			ListBuffer[AbsDecision](new HaltDecision(this, player.play(this)))
		}
	}

	def invoke(p: ImmutablePlayer): ImmutablePlayer = new ImmutablePlayer(
																	p.supply,
																	p.discard,
																	p.field.addOne(this),
																	p.hand.removeOne(this), // Invoke is overriden in classes where hand.removeOne(this) won't work (i.e. Chapel, Cellar, Workshop, etc.)
																	p.deck,
																	p.actions - 1 + actions,
																	p.buys + buys,
																	p.coin + coin,
																	p.topOfDeck,
																	p.needsToDraw + cards)
}



// Dummy class to avoid Option[Action] in the action field of a Decision.
case class NoAction() extends Action(0) {
	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = ListBuffer[AbsDecision]()
	override def invoke(p: ImmutablePlayer): ImmutablePlayer = p
}
