package dominion.src

import scala.collection.immutable.HashMap

class DecisionMatrix(val actionVectors: HashMap[Card, ActionVector]) {
	
	def multiply(cardSet: CardSet): ActionVector = {
		var vector = new ActionVector
		for ((card, count) <- cardSet.cards) {
			if (actionVectors.contains(card) && count > 0) {
				vector = vector.add(actionVectors(card))
			}
		}
		return vector
	}

	def mutate(shift: Double): DecisionMatrix = {
		var nextMap = HashMap[Card, ActionVector]()
		for ((card, vector) <- actionVectors) {
			nextMap += ((card, vector.mutate(shift)))
		}
		return new DecisionMatrix(nextMap)
	}

	override def toString: String = {
		"Do later if you really need to."
	}

	def toJSON: String = {
		var aggregate = "{\n"
		var isFirst = true
		for ((card, vector) <- actionVectors) {
			if (!isFirst) {
				aggregate += ",\n"
			} else {
				isFirst = false
			}

			aggregate += "\"" + card.plain + "\":" + vector.toJSON
		}
		return aggregate + "\n}"
	}
}

object DecisionMatrix {
	def randomMatrix: DecisionMatrix = {

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

		var map = HashMap[Card, ActionVector]()
		for (card <- allCards) {
			map += ((card, ActionVector.randomVector))
		}

		return new DecisionMatrix(map)
	}

	def parseJSON(m: Map[String, Any]): DecisionMatrix = {
		var map = HashMap[Card, ActionVector]()
		for ((key, value) <- m) {
			val card = CardSet.makeCard(key)
			value match {
				case vect: Map[String, Double] =>
					map += ((card, ActionVector.makeVector(vect)))
				case _ => throw new BadJSONException()
			}
		}
		return new DecisionMatrix(map)
	}
}
