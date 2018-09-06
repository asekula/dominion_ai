package dominion.src


class ImmutablePlayer(
		val supply: CardSet,
		val discard: CardSet,
		val field: CardSet, 
		val hand: CardSet, 
		val deck: CardSet, 
		val actions: Int,
		val buys: Int,
		val coin: Int,
		val topOfDeck: List[Card],
		val needsToDraw: Int) {

	// A CardSet of all the cards that the player owns.
	def allCards: CardSet = discard.addAll(hand).addAll(field).addAll(deck).addAllList(topOfDeck)

	// A set of the unique actions in the hand.
	def uniqueActions: Set[Action] = hand.cards.keySet.filter(a => a.isInstanceOf[Action]).map(a => a.asInstanceOf[Action])
	
	// Plays the action by making a new Immutable Player.
	def play(a: Action): ImmutablePlayer = a.invoke(this)

	// Moves the discard pile into the deck. Used from drawing cards, when the deck is empty.
	def moveDiscardToDeck: ImmutablePlayer = new ImmutablePlayer(
														supply, 
														new CardSet, 
														field,
														hand,
														discard,
														actions,
														buys,
														coin,
														topOfDeck,
														needsToDraw)

	// Moves the card from the deck to the hand.
	def moveToHand(card: Card): ImmutablePlayer = new ImmutablePlayer(
															supply, 
															discard, 
															field,
															hand.addOne(card),
															deck.removeOne(card),
															actions,
															buys,
															coin,
															topOfDeck,
															needsToDraw)

	// Discards a set of cards.
	def discard(cardSet: CardSet): ImmutablePlayer = new ImmutablePlayer(
															supply, 
															discard.addAll(cardSet), 
															field,
															hand.removeAll(cardSet),
															deck,
															actions,
															buys,
															coin,
															topOfDeck,
															needsToDraw)
	
	// For curses.
	def addToDiscard(card: Card): ImmutablePlayer = 
		if (supply.contains(card)) {
			new ImmutablePlayer(
							supply.removeOne(card), 
							discard.addOne(card), 
							field,
							hand,
							deck,
							actions,
							buys,
							coin,
							topOfDeck,
							needsToDraw)
		} else {
			this
		}
		

	def addToDrawNumber(num: Int): ImmutablePlayer = new ImmutablePlayer(
															supply, 
															discard, 
															field,
															hand,
															deck,
															actions,
															buys,
															coin,
															topOfDeck,
															needsToDraw + num)

	def copy: ImmutablePlayer = new ImmutablePlayer(
												supply, 
												discard, 
												field,
												hand,
												deck,
												actions,
												buys,
												coin,
												topOfDeck,
												needsToDraw)

	def withoutDiscard: ImmutablePlayer = new ImmutablePlayer(
												supply, 
												new CardSet, 
												field,
												hand,
												deck,
												actions,
												buys,
												coin,
												topOfDeck,
												needsToDraw)

	def nextRound: ImmutablePlayer = new ImmutablePlayer(
												supply, 
												discard.addAll(field.clear).addAll(hand.clear), 
												new CardSet,
												new CardSet,
												deck,
												1,
												1,
												0,
												topOfDeck,
												5)

	def setSupply(newSupply: CardSet): ImmutablePlayer = new ImmutablePlayer(
															newSupply, 
															discard, 
															field,
															hand,
															deck,
															actions,
															buys,
															coin,
															topOfDeck,
															needsToDraw)

	def putOnDeck(card: Card): ImmutablePlayer = new ImmutablePlayer(
															supply, 
															discard, 
															field,
															hand.removeOne(card),
															deck,
															actions,
															buys,
															coin,
															card :: topOfDeck,
															needsToDraw)

	// Moves all the cards in *cards* from the deck to the hand. Used in drawing cards.
	def fromDeckToHand(moveThese: CardSet): ImmutablePlayer = new ImmutablePlayer(
																				supply, 
																				discard, 
																				field,
																				hand.addAll(moveThese),
																				deck.removeAll(moveThese),
																				actions,
																				buys,
																				coin,
																				topOfDeck,
																				needsToDraw)

	// Important: Draws *randomly*. Not used in the api. Only used for genetic algorithm.
	def draw: ImmutablePlayer = {
		if (needsToDraw <= topOfDeck.size) {
			new ImmutablePlayer(
							supply, 
							discard, 
							field,
							hand.addAllList(topOfDeck.take(needsToDraw)),
							deck,
							actions,
							buys,
							coin,
							topOfDeck.drop(needsToDraw),
							0)
		} else {
			val numFromDeck = needsToDraw - topOfDeck.size
			if (deck.size >= numFromDeck) {
				val cardsFromDeck = HandGenerator.randomHand(deck, numFromDeck)
				new ImmutablePlayer(
							supply, 
							discard, 
							field,
							hand.addAll(cardsFromDeck.addAllList(topOfDeck)),
							deck.removeAll(cardsFromDeck),
							actions,
							buys,
							coin,
							List(),
							0)
			} else {
				val restOfDeck = deck.addAllList(topOfDeck)
				val numFromDiscard = needsToDraw - restOfDeck.size
				if (discard.size >= numFromDiscard) {
					val cardsFromDiscard = HandGenerator.randomHand(discard, numFromDiscard)
					new ImmutablePlayer(
							supply, 
							new CardSet, 
							field,
							hand.addAll(restOfDeck.addAll(cardsFromDiscard)),
							discard.removeAll(cardsFromDiscard),
							actions,
							buys,
							coin,
							List(),
							0)
				} else {
					//throw new Exception("Not enough cards in deck.")
					new ImmutablePlayer(
							supply, 
							new CardSet, // discard
							field,
							hand.addAll(deck).addAll(discard).addAllList(topOfDeck),
							new CardSet, // deck
							actions,
							buys,
							coin,
							List(), // topOfDeck.
							0) // Can't draw any more...continues game.

					//return this
				}
			}
		}
	}

	/**
	  * Supply is the starting supply. Used to check if the current supply has three less stacks.
	  */
	def gameOver(supply: CardSet): Boolean = {
		val isOver = ((!this.supply.cards.contains(new Province)) || 
			(this.supply.cards.keySet.size <= supply.cards.keySet.size - 3))

		//if (isOver) {
			//println("Decks gone: " + (supply.cards.keySet &~ this.supply.cards.keySet))
		//}

		return isOver
	}

	// Displays the player nicely.
	override def toString: String = {
		var s = ""
		s += "\nSupply: " + supply.toString
		s += "\nDiscard: " + discard.toString
		s += "\nField: " + field.toString
		s += "\nHand: " + hand.toString
		s += "\nDeck: " + deck.toString
		s += "\nActions: " + actions
		s += "\nBuys: " + buys
		s += "\nCoin: " + coin
		s += "\nTop Of Deck: " + topOfDeck
		s += "\nStill needs to draw " + needsToDraw + " cards.\n"
		return s
	}

	// Computes total coin + treasures in a hand. Used to figure out what the player can buy.
	//coin + hand.cards.keys.filter(a => a.isInstanceOf[TreasureCard]).map(a => a.asInstanceOf[TreasureCard].value * hand.cards(a)).sum	
	def handValue: Int = {
		var total = coin
		for((card, count) <- hand.cards) {
			if (card.isInstanceOf[TreasureCard]) {
				total += (card.asInstanceOf[TreasureCard].value * count)
			}
		}

		total
	}

	// Average cost of cards in the map, treating victory cards as costing 0.
	def avgCostNonVictory(map: CardSet): Double = {
		var value: Double = 0
		var total: Double = 0
		for ((card, count) <- map.cards) {
			if (!card.isInstanceOf[VictoryCard]) {
				value += (card.cost * count)
			}
			total += count
		}

		if (total == 0) 0 
		else value / total
	}

	// Of victory cards
	def totalPoints: Int = allCards.totalPoints
}
