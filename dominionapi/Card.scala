package dominion.src


abstract class Card(val cost: Int) {
	override def toString: String = getClass().getSimpleName()
	def clear: Card = this

	// The english description of the class, displayed as an instruction.
	def toEnglish: String = toString

	// This is the string representation used in the app. Copper is "Copper" and Remodel(_) is "Remodel"
	// Used importantly in CardSet.makeJSON.
	def plain: String = getClass().newInstance.asInstanceOf[Card].toString
}

abstract class SimpleCard(override val cost: Int, val value: Int) extends Card(cost)

class TreasureCard(override val cost: Int, override val value: Int) extends SimpleCard(cost, value)
class VictoryCard(override val cost: Int, override val value: Int) extends SimpleCard(cost, value)

case class Copper() extends TreasureCard(0, 1)
case class Silver() extends TreasureCard(3, 2)
case class Gold() extends TreasureCard(6, 3)

case class Curse() extends VictoryCard(0, -1)
case class Estate() extends VictoryCard(2, 1)
case class Duchy() extends VictoryCard(5, 3)
case class Province() extends VictoryCard(8, 6)
case class Gardens() extends VictoryCard(4, 0) {
	def value(cardsInDeck: Int): Int = cardsInDeck / 10 // will be Int.
}