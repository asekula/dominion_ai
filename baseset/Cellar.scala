package dominion.src

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

// Q: Pattern matching in constructor -- too ugly?
case class Cellar(discard: Option[CardSet]) extends Action(2, 1, 0, discard match {case Some(c) => c.size case _ => 0}, 0) {
	
	def this() { this(None) }

	override def clear: Card = new Cellar()


	override def decisions(player: ImmutablePlayer): ListBuffer[AbsDecision] = {
		val next = ListBuffer[AbsDecision]()
		var toConsider = player.hand.removeOne(this)

		for (combination <- HandGenerator.allCombinations(toConsider)) { // combination is an immutable HashMap.
			val nextCellar = new Cellar(Some(combination))
			
			if (combination.size == 0) {
				next += new ContinueDecision(nextCellar, player.play(nextCellar))
			} else {
				next += new HaltDecision(nextCellar, player.play(nextCellar))
			}
		}

		next
	}

	override def invoke(p: ImmutablePlayer): ImmutablePlayer = discard match {
		case None => p
		case Some(cards) => new ImmutablePlayer(
											p.supply, 
											p.discard.addAll(cards), 
											p.field.addOne(this), 
											p.hand.removeOne(new Cellar(None)).removeAll(cards),
											p.deck,
											p.actions - 1 + actions,
											p.buys, 
											p.coin,
											p.topOfDeck,
											cards.size) // If deciding on Cellar, won't need to add to numToDraw.
	}

	override def toString: String = discard match {
		case None => "Cellar"
		case Some(cards) => "Cellar(discard: " + cards + ")"
	}

	override def toEnglish: String = discard match {
		case None => "Cellar"
		case Some(cards) =>	"Cellar: discard " + cards.toEnglish + "."
	}
}