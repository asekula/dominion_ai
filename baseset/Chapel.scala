package dominion.src

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class Chapel(trash: Option[CardSet]) extends Action(2) {

	def this() { this(None) }

	override def clear: Card = new Chapel()


	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val next = ListBuffer[AbsDecision]()
		var cardOptions = player.hand.removeOne(this)

		for (combination <- HandGenerator.combinationsUpToNumber(cardOptions, 4)) { // combination is a CardSet.
			val nextChapel = new Chapel(Some(combination))
			next += new ContinueDecision(nextChapel, player.play(nextChapel))
		}

		next
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = trash match {
		case None => p
		case Some(cards) => new ImmutablePlayer(
											p.supply, 
											p.discard, // Doesn't add to discard, trashes instead.
											p.field.addOne(this), 
											p.hand.removeOne(new Chapel(None)).removeAll(cards),
											p.deck,
											p.actions - 1,
											p.buys, 
											p.coin,
											p.topOfDeck,
											0)
	}

	override def toString: String = trash match {
		case None => "Chapel"
		case Some(cards) => "Chapel(trash: " + cards + ")"
	}

	override def toEnglish: String = trash match {
		case None => "Chapel"
		case Some(cards) => "Chapel: trash " + cards + "."
	}
}