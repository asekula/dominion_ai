package dominion.src

import scala.collection.immutable.HashMap

class ActionVector(val actionCards: HashMap[Action, Double]) {

	def this() {
		this(new HashMap[Action, Double]())
	}

	// Important: Treating the existence of a card as 1

	def dot(cardSet: CardSet): ActionVector = {
		new ActionVector(actionCards.filter(a => cardSet.cards.contains(a._1) && cardSet.cards(a._1) > 0))
	}

	def max: (Action, Double) = actionCards.maxBy(_._2)

	def sum: Double = actionCards.map(a => a._2).sum

	def add(vector: ActionVector): ActionVector = {
		var added = HashMap[Action, Double]()
		for (key <- vector.actionCards.keySet.union(actionCards.keySet)) {
			var total = 0.0
			if (vector.actionCards.contains(key)) {
				total += vector.actionCards(key)
			}
			if (actionCards.contains(key)) {
				total += actionCards(key)
			}
			if (total > 0) {
				added += ((key, total))
			}
		}
		return new ActionVector(added)
	}

	def remove(key: Action): ActionVector = {
		new ActionVector(actionCards - key)
	}

	def mutate(shift: Double): ActionVector = {
		var nextMap = new HashMap[Action, Double]()

		for ((card, count) <- actionCards) {
			var nextCount = 0.0
			
			if (math.random > 0.5) nextCount = math.min(count + shift, 1.0)
			else nextCount = math.max(count - shift, 0.0)
			nextMap += ((card, nextCount))
		}

		return new ActionVector(nextMap)
	}

	def toJSON: String = {
		var aggregate = "{\n"
		var isFirst = true
		for ((card, count) <- actionCards) {
			if (!isFirst) {
				aggregate += ",\n"
			} else {
				isFirst = false
			}

			aggregate += "\"" + card.plain + "\":" + count
		}
		return aggregate + "\n}"
	}
}

object ActionVector {
	def randomVector: ActionVector = {
		val allActions = List[Action](
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
								new Bureaucrat)

		var map = HashMap[Action, Double]()
		for (card <- allActions) {
			map += ((card, math.random))
		}

		return new ActionVector(map)
	}

	def makeVector(m: Map[String, Double]): ActionVector = {
		var hash = HashMap[Action, Double]()
		for ((cardString, count) <- m) {
			hash += ((CardSet.makeCard(cardString).asInstanceOf[Action], count))
		}
		return new ActionVector(hash)
	}
}