package dominion.src

import scala.collection.mutable.ListBuffer


case class Remodel(val trash: Option[Card], val gain: Option[Card]) extends Action(4) {

	def this() { this(None, None) }

	override def clear: Card = new Remodel() // There's got to be a better way.

	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val next = ListBuffer[AbsDecision]()
		val validCards = player.hand.cards.keySet - this // Hand.keySet is an immutable Set.
		 
		for (t <- validCards) {
			for(g <- player.supply.withinPrice(t.cost + 2)) {
				val nextRemodel = new Remodel(Some(t), Some(g))
				next += new ContinueDecision(nextRemodel, player.play(nextRemodel))
			}
		}

		next
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = (trash, gain) match {
		case (Some(t), Some(g)) => new ImmutablePlayer(
											p.supply.removeOne(g),
											p.discard.addOne(g),
											p.field.addOne(this),
											p.hand.removeOne(new Remodel(None, None)).removeOne(t),
											p.deck,
											p.actions - 1,
											p.buys,
											p.coin,
											p.topOfDeck,
											0)
		case _ => p
	}

	override def toString: String = (trash, gain) match {
		case (Some(t), Some(g)) =>
			getClass.getSimpleName + "(trash: " + t + ", gain: " + g + ")"
		case _ => getClass.getSimpleName
	}

	override def toEnglish: String = (trash, gain) match {
		case (Some(t), Some(g)) =>
			"Remodel: trash a " + t.toEnglish + ", and gain a " + g.toEnglish + "."
		case _ => "Remodel"
	}
}
