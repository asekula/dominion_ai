package dominion.src

import scala.collection.immutable.HashMap
import scala.util.parsing.json._

/**
  * Immutable class that represents sets of cards.
  * Implementation uses a HashMap.
  * Basically is a Set that allows duplicates, but does not have ordering.
  * Using double to allow CardVector to store doubles.
  *
  * @param cards - the hashmap of cards.
  */
class CardSet(val cards: HashMap[Card, Int]) {

	val size: Int = cards.values.sum

	/**
	  * Creates an empty cardSet.
	  */
	def this() {
		this(HashMap[Card, Int]())
	}

	def this(map: Map[Card, Int]) {
		this(CardSet.makeHashMap(map))
	}
	
	/**
	  * Removes a single card from the CardSet.
	  *
	  * @param c - the card
	  * @return - the resulting CardSet
	  */
	def removeOne(c: Card): CardSet = removeANumber(c, 1)

	/**
	  * Removes a number of a certain card.
	  *
	  * @param c - the card
	  * @param num - the number of cards
	  * @return the resulting CardSet
	  */
	def removeANumber(c: Card, num: Int): CardSet = {
		if (cards.contains(c)) {
			if (cards(c) > num) new CardSet(cards + ((c, cards(c) - num)))
			else new CardSet(cards - c)
		} else this
	}

	/**
	  * Removes all cards in the given cardSet.
	  * 
	  * @param toRemove - the cardSet to remove
	  * @return - the resulting CardSet
	  */
	def removeAll(toRemove: CardSet): CardSet = {
		var updated = this
		for ((card, count) <- toRemove.cards) {
			updated = updated.removeANumber(card, count)
		}

		return updated
	}

	/**
	  * Adds a single card to the set.
	  * 
	  * @param c - the card
	  * @return - the cardset with the card
	  */
	def addOne(c: Card): CardSet = addANumber(c, 1)


	/**
	  * Adds a number of cards to the set.
	  * 
	  * @param c - the card
	  * @param num - the number to add
	  * @return - the resulting cardset
	  */
	def addANumber(c: Card, num: Int): CardSet = {
		if (cards.contains(c)) new CardSet(cards + ((c, cards(c) + num)))
		else new CardSet(cards + ((c, num)))
	}


	/**
	  * Adds all cards in the given cardSet.
	  * 
	  * @param toAdd - the cardSet to add
	  * @return - the resulting CardSet
	  */
	def addAll(toAdd: CardSet): CardSet = {
		var updated = this
		for ((card, count) <- toAdd.cards) {
			updated = updated.addANumber(card, count)
		}

		return updated
	}

	/**
	  * Adds all cards in the given list.
	  * 
	  * @param toAdd - the list to add
	  * @return - the resulting CardSet
	  */
	def addAllList(toAdd: List[Card]): CardSet = {
		if (toAdd.length == 0) this
		else addAllList(toAdd.tail).addOne(toAdd.head)
	}

	/** 
	  * Removes a key from the cardset.
	  * 
	  * @param key - the key to remove
	  * @return - the resulting cardSet
	  */
	def removeKey(key: Card): CardSet = new CardSet(cards - key)

	/**
	  * Adds a key-value pair to the cardset.
	  *
	  * @param pair - a card, int tuple
	  * @return - the cardset including the kv pair
	  */
	def addPair(pair: (Card, Int)): CardSet = new CardSet(cards + pair)

	/**
	  * All cards in the cardset costing less than or equal to the input price.
	  *
	  * @param price - the input price
	  * @return - the cards in the cardset consting less than or equl to the given price.
	  */
	def withinPrice(price: Int): Set[Card] = cards.keySet.filter(a => (cards(a) > 0 && a.cost <= price))

	/**
	  * All cards in the supply costing within the price range. Inclusive.
	  *
	  * @param low - the lower bound
	  * @param high - the upper bound
	  * @return - the set of cards
	  */
	def withinPrice(low: Int, high: Int): Set[Card] = cards.keySet.filter(a => (cards(a) > 0 && a.cost >= low && a.cost <= high))

	/**
	  * All cards in the cardset costing less than or equal to the input price.
	  *
	  * @param high - the input price
	  * @return - the cards in the cardset consting less than or equl to the given price.
	  */
	def mapWithinPrice(high: Int): CardSet = {
		var updated = HashMap[Card, Int]()
		for (c <- withinPrice(high)) {
			updated = updated + ((c, cards(c)))
		}
		return new CardSet(updated)
	}
	
	/**
	  * All cards in the supply costing within the price range. Inclusive.
	  *
	  * @param low - the lower bound
	  * @param high - the upper bound
	  * @return - the cardset
	  */
	def mapWithinPrice(low: Int, high: Int): CardSet = {
		var updated = HashMap[Card, Int]()
		for (c <- withinPrice(low, high)) {
			updated = updated + ((c, cards(c)))
		}
		return new CardSet(updated)
	}
	
	override def toString: String = cards.toString

	/**
	  * The nth card in the set.
	  * 
	  * @param n - the card number in the set.
	  * @return - the nth card
	  */
	def nthCard(n: Int): Card = {
		var aggregateCount = 0
		for ((c, count) <- cards) {
			aggregateCount += count
			if (aggregateCount >= n) return c
		}
		throw new IndexOutOfBoundsException
		return null // Will result in exception thrown somewhere.
	}

	def clear: CardSet = {
		var updated = HashMap[Card, Int]()
		for ((c, count) <- cards) {
			updated = updated + ((c.clear, count))
		}
		return new CardSet(updated)
	}

	def contains(card: Card): Boolean =
		if (cards.contains(card)) cards(card) > 0
		else false

	// Of Victory Cards only.
	def totalPoints: Int = {
		var total = 0
		for ((c, count) <- cards) {
			if (c.isInstanceOf[VictoryCard]) {
				total += (count * c.asInstanceOf[VictoryCard].value) // Todo: Make sure Gardens is evaluated correctly.
			}
		}
		return total
	}

	def toJSON: String = {
		var aggregate = "{"
		var notFirst = false
		for ((c, count) <- cards) {
			if (notFirst) aggregate += ", "
			else notFirst = true

			aggregate += "\"" + c.plain + "\": " + count 
		}
		return aggregate + "}"
	}

	def toEnglish: String = {

		if (cards.size == 0) {
			return "nothing"
		}

		var aggregate = ""
		var notFirst = false
		for ((c, count) <- cards) {
			if (notFirst) aggregate += ", "
			else notFirst = true

			aggregate += count + " " + c.plain

			if (count > 1) aggregate += "s"
		}
		return aggregate
	}

	def toValueDistribution: ValueDistribution = {
		var map = HashMap[Card, Double]()
		var totalCards = 0
		for ((card, count) <- cards) {
			map += ((card, count.toDouble / size.toDouble))
			totalCards += 1
		}
		return new ValueDistribution(map, totalCards)
	}
}

object CardSet {

	def makeCardSet(json: Any): CardSet = {
		json match {
			case m: Map[String, Double] =>
				return new CardSet(m map {case (key, value) => (CardSet.makeCard(key), value.toInt)})
			case _ =>
		}

		throw new BadJSONException()
		return new CardSet()
	}

	def makeCard(s: String): Card = {
		//if (stringToCards.contains(s)) return stringToCards(s)
		return Class.forName("dominion.src." + s.filter(a => a != ' ')).newInstance.asInstanceOf[Card] // This is where the magic happens.
	}

	def makeHashMap(m: Map[Card, Int]): HashMap[Card, Int] = {
		var hash = new HashMap[Card, Int]()
		m.foreach(a => hash += a)
		return hash
	}
}