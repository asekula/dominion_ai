package dominion.src

import scala.collection.mutable.ListBuffer

case class Moneylender() extends Action(4) {

	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		if (player.hand.cards.keySet.contains(new Copper)) {
			ListBuffer[AbsDecision](new ContinueDecision(this, player.play(this)))
		} else ListBuffer[AbsDecision]()
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = {
		if (p.hand.cards.keySet.contains(new Copper)) {
			new ImmutablePlayer(
							p.supply, 
							p.discard, 
							p.field.addOne(this), 
							p.hand.removeOne(this).removeOne(new Copper),
							p.deck,
							p.actions - 1,
							p.buys, 
							p.coin + 3,
							p.topOfDeck,
							0)
		} else p
	}
}