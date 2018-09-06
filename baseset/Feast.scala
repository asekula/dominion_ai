package dominion.src

import scala.collection.mutable.ListBuffer


case class Feast(val gain: Option[Card]) extends Action(4) {

	def this() { this(None) }

	override def clear: Card = new Feast()


	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val next = ListBuffer[AbsDecision]()
		
		for(g <- player.supply.withinPrice(5)) {
			val nextFeast = new Feast(Some(g))
			next += new ContinueDecision(nextFeast, player.play(nextFeast))
		}

		next
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = gain match {
		case Some(g) => new ImmutablePlayer(
										p.supply.removeOne(g),
										p.discard.addOne(g),
										p.field,
										p.hand.removeOne(new Feast(None)),
										p.deck,
										p.actions - 1,
										p.buys,
										p.coin,
										p.topOfDeck,
										0)
		case None => p
	}

	override def toString: String = gain match {
		case Some(g) => "Feast(gain:" + g + ")"
		case None => "Feast"
	}

	override def toEnglish: String = gain match {
		case Some(g) => "Feast: gain a " + g.toEnglish + "."
		case None => "Feast"
	}
}
