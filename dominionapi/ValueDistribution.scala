package dominion.src

import scala.collection.immutable.HashMap

// All doubles in map add up to 1.
class ValueDistribution(val map: HashMap[Card, Double], focusNumber: Int) {


	// Important: Mutating only changes the values in the distribution,
	// it doesn't change what cards are in the distribution.
	// You can never mutate and get another card in the distribution.
	def mutate(shift: Double): ValueDistribution = {
		
		val shiftEachBy = shift / focusNumber.toDouble // Arbitrary, as long as it's not that big.

		var tempMap = HashMap[Card, Double]()
		var total = 0.0
		for ((card, count) <- tempMap) {
			val randomSign = (scala.util.Random.nextInt(2) * 2) - 1 // -1 or 1.
			val nextCount = math.max(-1, count + (randomSign * shiftEachBy)) // Note: Change this here from max(0,...) to max(-1) to allow for negative values. (i.e. trashing)
			total += nextCount
			tempMap += ((card, nextCount))
		}

		var map = HashMap[Card, Double]()
		for ((card, count) <- map) {
			map += ((card, count / total))
		}

		return new ValueDistribution(map, focusNumber)
	}

	def asValueFunction: (ImmutablePlayer => Double) =
		return (a => -1 * a.allCards.toValueDistribution.distanceTo(this))
	// -1 because we want to increase the value function, i.e. lower the distance.

	def distanceTo(another: ValueDistribution): Double = {
		val shared = map.keys.toSet.intersect(another.map.keys.toSet)
		val total = map.keys.toSet.union(another.map.keys.toSet)
		var distance = 0.0

		for (card <- total) {
			if (shared(card)) {
				val translation = (map(card) - another.map(card))
				distance += (translation * translation)
			} else if (map.contains(card)) {
				distance += (map(card) * map(card))
			} else {
				distance += (another.map(card) * another.map(card))
			}
		}

		return distance
	}

	def toJSON: String = {
		var aggregate = "{\n"
		var isFirst = true
		for ((card, count) <- map) {
			if (!isFirst) {
				aggregate += ",\n"
			} else {
				isFirst = false
			}

			aggregate += "\"" + card.plain + "\":" + count
		}
		return aggregate + "\n}"
	}

	override def toString: String = map.toString
}

object ValueDistribution {
	def randomDistribution: ValueDistribution = {
		val allCards = List[Card](
								new Mine,
								new Laboratory,
								new Witch,
								new Village,
								new Market,
								new Moneylender,
								new Chapel,
								new Cellar,
								new Remodel,
								new Militia,
								new Workshop,
								new Smithy,
								new Woodcutter,
								new Festival,
								new CouncilRoom,
								new Moat,
								new Feast,
								new Chancellor,
								new Bureaucrat,
								new Copper,
								new Silver,
								new Gold,
								new Curse,
								new Estate,
								new Duchy,
								new Province)


		val randomFocusNumber = scala.util.Random.nextInt(8) + 2 // Won't include 10. Made this larger because negatives.
		val shuffled = util.Random.shuffle(allCards).take(randomFocusNumber)
		var totalDistr = 0.0
		var tempmap = HashMap[Card, Double]()

		// Note: Allowing negatives might get weird.

		for (card <- shuffled) {
			val rand = math.random - 0.5 // All variables have the same distribution. Important: CAN BE NEGATIVE.
			// They still all add to 1.
			tempmap += ((card, rand))
			totalDistr += rand
		}

		var map = HashMap[Card, Double]() // Scaled to 1.
		for ((card, count) <- tempmap) {
			map += ((card, count / totalDistr))
		}



		return new ValueDistribution(map, randomFocusNumber)
	}
}