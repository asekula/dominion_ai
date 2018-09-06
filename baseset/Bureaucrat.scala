package dominion.src

import scala.collection.mutable.ListBuffer

case class Bureaucrat() extends ActionAttack(4, 0, 0, 0, 0) {
	override def invoke(p: ImmutablePlayer): ImmutablePlayer = {
		if (p.supply.cards.keySet.contains(new Silver)) {
			new ImmutablePlayer(
							p.supply.removeOne(new Silver),
							p.discard,
							p.field.addOne(this), 
							p.hand.removeOne(this),
							p.deck, 
							p.actions - 1, 
							p.buys,
							p.coin,
							new Silver :: p.topOfDeck,
							0)
		} else super.invoke(p)
	}

	def opponentActions(player: ImmutablePlayer): ListBuffer[OpponentAction] = {
		val vcs = player.hand.cards.keySet.filter(c => c.isInstanceOf[VictoryCard])
		if (!player.hand.cards.contains(new Moat) && vcs.size > 0) {
			val removeThis = vcs.head
			return ListBuffer(new PutOnDeck(removeThis))
		} else {
			return ListBuffer(new NoOpponentAction)
		}
	}
}