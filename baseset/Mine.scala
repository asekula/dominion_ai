package dominion.src

import scala.collection.mutable.ListBuffer


case class Mine(val trash: Option[Card], val gain: Option[Card]) extends Action(5) {
		
	def this() { this(None, None) }

	override def clear: Card = new Mine()

	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val next = ListBuffer[AbsDecision]()

		if (player.hand.cards.contains(new Copper()) && player.supply.cards.contains(new Silver())) {
			val nextMine = new Mine(Some(new Copper), Some(new Silver))
			next += new ContinueDecision(nextMine, player.play(nextMine))
		}

		if (player.hand.cards.contains(new Silver()) && player.supply.cards.contains(new Gold())) {
			val nextMine = new Mine(Some(new Silver), Some(new Gold))
			next += new ContinueDecision(nextMine, player.play(nextMine))
		}

		next
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = (trash, gain) match {
		case (Some(t), Some(g)) => new ImmutablePlayer(
											p.supply.removeOne(g),
											p.discard,
											p.field.addOne(this),
											p.hand.removeOne(new Mine(None, None)).removeOne(t).addOne(g),
											p.deck,
											p.actions - 1,
											p.buys,
											p.coin,
											p.topOfDeck,
											0)
		case _ => p
	}

	override def toString: String = {
		(trash, gain) match {
			case (Some(t), Some(g)) =>
				getClass.getSimpleName + "(trash: " + t + ", gain: " + g + ")"
			case _ => getClass.getSimpleName
		}
	}

	override def toEnglish: String = {
		(trash, gain) match {
			case (Some(t), Some(g)) =>
				"Mine: trash a " + t.toEnglish + ", and gain a " + g.toEnglish + "."
			case _ => "Mine"
		}
	}
}