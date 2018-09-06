package dominion.src

import scala.collection.mutable.ListBuffer


case class Workshop(val gain: Option[Card]) extends Action(4) {
	
	def this() { this(None) }

	override def clear: Card = new Workshop()


	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val next = ListBuffer[AbsDecision]()

		for (card <- player.supply.withinPrice(4)) {
			val nextWorkshop = new Workshop(Some(card))
			next += new ContinueDecision(nextWorkshop, player.play(nextWorkshop))
		}

		next
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = gain match {
		case Some(card) => new ImmutablePlayer(
											p.supply.removeOne(card),
											p.discard.addOne(card),
											p.field.addOne(this),
											p.hand.removeOne(new Workshop(None)),
											p.deck,
											p.actions - 1,
											p.buys,
											p.coin,
											p.topOfDeck,
											0)
		case None => p
	}

	override def toString: String = gain match {
		case Some(g) => "Workshop(" + g + ")"
		case None => "Workshop"
	}


	override def toEnglish: String = gain match {
		case Some(g) => "Workshop: gain a " + g.toEnglish + "."
		case None => "Workshop"
	}
}
