package dominion.src


abstract class OpponentAction extends Action(0) { 
	override def invoke(player: ImmutablePlayer): ImmutablePlayer
}

case class NoOpponentAction() extends OpponentAction {
	override def invoke(player: ImmutablePlayer): ImmutablePlayer = player
	override def toEnglish: String = "Do nothing."
}

case class DrawCards(draw: Int) extends OpponentAction {
	override def invoke(player: ImmutablePlayer): ImmutablePlayer = player.addToDrawNumber(draw)

	override def toString: String = "DrawCards(number: " + draw + ")"
	override def toEnglish: String = {
		var extra = "."
		if (draw > 1) extra = "s."
		"Draw " + draw + " card" + extra
	}
}

case class Discard(toDiscard: CardSet) extends OpponentAction {
	override def invoke(player: ImmutablePlayer): ImmutablePlayer = player.discard(toDiscard)

	override def toString: String = "Discard(cards: " + toDiscard + ")"
	override def toEnglish: String = "Discard " + toDiscard.toEnglish + "."
}

case class PutOnDeck(toPut: Card) extends OpponentAction {
	override def invoke(player: ImmutablePlayer): ImmutablePlayer = player.putOnDeck(toPut)

	override def toString: String = "PutOnDeck(card: " + toPut + ")"
	override def toEnglish: String = "Put a " + toPut.toEnglish + " on top of your deck."
}

case class AddToDiscard(card: Card) extends OpponentAction {
	override def invoke(player: ImmutablePlayer): ImmutablePlayer = player.addToDiscard(card)

	override def toString: String = "AddToDiscard(card: " + card + ")"
	override def toEnglish: String = "Add a " + card.toEnglish + " to your discard pile."
}