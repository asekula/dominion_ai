package dominion.src

import scala.collection.mutable.ListBuffer
import scala.math

// Todo: Remove redundant code.

object HandGenerator {

	def generateHands(player: ImmutablePlayer, draw: Int): ListBuffer[(ImmutablePlayer, Double)] = {

		if (draw == 0) return ListBuffer((player, 1.0))

		if (player.discard.size + player.deck.size < draw) throw new IllegalArgumentException("Cannot draw enough cards.")

		if (player.topOfDeck.length > 0) {
			var drawFromTop = draw
			if (player.topOfDeck.length < draw) drawFromTop = player.topOfDeck.length

			var newTopOfDeck = player.topOfDeck
			var newHand = player.hand
			for (i <- 1 to drawFromTop) {
				newHand.addOne(newTopOfDeck.head)
				newTopOfDeck = newTopOfDeck.tail
			}

			val newPlayer = new ImmutablePlayer(player.supply, player.discard, player.field, newHand, player.deck, 
												player.actions, player.buys, player.coin, newTopOfDeck, 0)
			generateHands(newPlayer, draw - drawFromTop)
		}

		val deckSize = player.deck.size
		if (deckSize < draw) {
			val exhaustDeck: ListBuffer[(ImmutablePlayer, Double)] = generateHands(player, deckSize)
			val choices = new ListBuffer[(ImmutablePlayer, Double)]()

			for ((p, prob) <- exhaustDeck) {
				val fromDiscard = generateHands(p.moveDiscardToDeck, draw - deckSize)
				choices ++= fromDiscard.map(a => ((a._1, a._2 * prob)))
			}

			return choices
		} else { // Enough cards in deck.
			return combinationsOfNumberWithProbability(player.deck, draw).map(a => (player.fromDeckToHand(a._1), a._2))
		}
	}

	// Maintaining invariant that map size is greater than num.
	def combinationsOfNumberWithProbability(deck: CardSet, num: Int): ListBuffer[(CardSet, Double)] = {
		val size = deck.size
		if (size < num) throw new IllegalArgumentException("Input map too small.")
		val onlyCombs = combinationsOfNumber(deck, num)
		onlyCombs.map(comb => (comb, probabilityOfCombination(deck, comb)))
	}

	def anyCombinationOfNumber(deck: CardSet, num: Int): CardSet = {
		if (num == 0) return new CardSet
		else {
			val key = deck.cards.keys.head
			val value = deck.cards(key)
			if (num <= value) (new CardSet).addANumber(key, num)
			else {
				anyCombinationOfNumber(deck.removeKey(key), num - value).addANumber(key, value)
			}
		}
	}
	
	def combinationsOfNumber(deck: CardSet, num: Int): ListBuffer[CardSet] = {
		if (num <= 0) return ListBuffer(new CardSet) // CurrentCount shouldn't be greater than num.
		else {
			val key = deck.cards.keys.head // Will throw error if doesn't exist, but map size will always be greater than num.
			val keyValue = deck.cards(key)
			val maxCountAllowed = math.min(num, keyValue)
			val minCountAllowed = math.max(0, num - (deck.size - keyValue)) // Pigeonhole: we may be forced to pick at least minCountAllowed items.

			val choices = new ListBuffer[CardSet]()

			for (count <- minCountAllowed to maxCountAllowed) {	
				val nextCombinations = combinationsOfNumber(deck.removeKey(key), num - count)
				for (comb <- nextCombinations) {
					if (count > 0) choices += new CardSet(comb.cards + ((key, count)))
					else choices += comb
				}
			}
			
			return choices
		}
	}

	def probabilityOfCombination(full: CardSet, chosen: CardSet): Double = {
		var p = 1.0
		for ((card, count) <- full.cards) {
			if (chosen.cards.contains(card)) {
				p *= choose(count, chosen.cards(card))
			}
		}
		
		p /= choose(full.size, chosen.size) // choose(n, k)
		return p
	}


	def choose(n: Int, k: Int): Int = Memoization.choose(n: Int, k: Int)


	def combinationsUpToNumber(deck: CardSet, num: Int): ListBuffer[CardSet] = {
		if (num == 0 || deck.cards.keySet.size == 0) ListBuffer(new CardSet)
		else {
			val key = deck.cards.keys.head
			val keyVal = deck.cards(key)
			val choices = new ListBuffer[CardSet]
			
			for (count <- 0 to math.min(keyVal, num)) {
				for (comb <- combinationsUpToNumber(deck.removeKey(key), num - count)) {
					if (count > 0) choices += comb.addPair((key, count))
					else choices += comb
				}
			}

			return choices
		}
	}

	def allCombinations(deck: CardSet): ListBuffer[CardSet] = {
		if (deck.cards.keySet.size > 0) {
			val key: Card = deck.cards.keys.head
			val choices = new ListBuffer[CardSet]
			
			for (count <- 0 to deck.cards(key)) {
				for (comb <- allCombinations(deck.removeKey(key))) {
					if (count > 0) choices += comb.addPair((key, count))
					else choices += comb
				}
			}

			return choices
		} else return ListBuffer(new CardSet)
	}

	def allPossibleBuys(supply: CardSet, coin: Int, buys: Int): ListBuffer[CardSet] = {
		if (buys == 0 || supply.cards.keySet.size == 0) ListBuffer(new CardSet)
		else {
			val possibleBuys = new ListBuffer[CardSet]
			val key: Card = supply.cards.keys.head
			var maxCountAllowed = buys
			if (key.cost != 0) maxCountAllowed = math.min(buys, coin / key.cost) // coin / key.cost will be an Int.

			for (count <- 0 to maxCountAllowed) {
				val restPossibleBuys = allPossibleBuys(supply.removeKey(key), coin - (key.cost * count), buys - count)
				if (count > 0) possibleBuys ++= restPossibleBuys.map(a => a.addPair((key, count)))
				else possibleBuys ++= restPossibleBuys
			}

			// if (possibleBuys.size == 0) { // Q: will this mess anything up?
			// 	possibleBuys += new CardSet
			// }

			return possibleBuys
		}
	}

	

	def randomHand(deck: CardSet, num: Int): CardSet = {
		val size = deck.size
		if (num == 0 || size == 0) return new CardSet

		val i = scala.util.Random.nextInt(size)
		val picked = deck.nthCard(i)
		return randomHand(deck.removeOne(picked), num - 1).addOne(picked)
	}
}