package dominion.src // Todo: (at some point) make packages that make sense.

import scala.util.parsing.json._

class SuggestionHandler(jsonString: String, bestRetriever: BestValueRetriever) {
	
	def makeResponse: String = {
		try {
			val json: Option[Any] = JSON.parseFull(jsonString)
			val player: ImmutablePlayer = makeImmutablePlayer(json)
			val id: String = getID(json)
			val suggestion: (ImmutablePlayer, List[String]) = getSuggestion(player, id) // Make sure actions are in english.
			val playerJson: String = playerToJSON(suggestion._1)
			val actions: String = cardListToJSON(suggestion._2) // Only used to tell the user what to do.

			return """{ "actions": """ + actions + """, "player": """ + playerJson + "}"
		} catch {
			case e: BadJSONException => return """ { "error": "bad json" } """
			case e: Exception => 
				println(e)
				return """ { "error": "internal error" } """
		}
	}

	private def getID(json: Option[Any]): String = {
		json match {
			case Some(m: Map[String, Any]) =>
				m("id") match {
					case s: String => s
					case _ => throw new BadRequestException()
				}
			case _ => throw new BadJSONException()
		}
	}

	private def cardListToJSON(actions: List[String]): String = {
		var aggregate = "["
		var first = true
		for (action <- actions) {
			if (!first) {
				aggregate += ", "
			} else {
				first = false
			}

			aggregate += "\"" + action + "\""
		}
		return aggregate + "]"
	}

	private def getSuggestion(player: ImmutablePlayer, id: String): (ImmutablePlayer, List[String]) = {
		var attack: Option[ActionAttack] = None
		id match {
			case "playRound" =>
				val decision = new ContinueDecision(player)

				val bestMatrixAndDistribution: (DecisionMatrix, ValueDistribution) = bestRetriever.getBest(player.supply)
				println("Best distr: " + bestMatrixAndDistribution._2)
				val best = decision.best(bestMatrixAndDistribution._1, bestMatrixAndDistribution._2.asValueFunction)
				val actions: List[Action] = best._2.tail // Remove no action.
				return (best._1, actions.map(a => a.toEnglish))

			case "militia" => attack = Some(new Militia)
			case "witch" => attack = Some(new Witch)
			case "bureaucrat" =>  attack = Some(new Bureaucrat)
			case "councilRoom" => attack  = Some(new CouncilRoom)
			case _ => 
				throw new BadRequestException()
				return (player, List()) // Need this?
		}

		attack match {
			case None => 
				throw new Exception
				return (player, List())
			case Some(actionAttack: ActionAttack) =>
				val decision = new OpponentDecision(actionAttack, player)
				val best = decision.best(DecisionMatrix.randomMatrix, Value.shallowValue)
				return (best._1, best._2.map(a => a.toEnglish))
		}
	}
	
	private def playerToJSON(player: ImmutablePlayer): String = """{
	"supply": """ + player.supply.toJSON + """,
	"discard": """ + player.discard.toJSON + """,
	"field": """ + player.field.toJSON + """,
	"hand": """ + player.hand.toJSON + """,
	"deck": """ + player.deck.toJSON + """,
	"actions": """ + player.actions + """,
	"buys": """ + player.buys + """,
	"coin": """ + player.coin + """,
	"topOfDeck": """ + cardListToJSON(player.topOfDeck.map(a => a.plain)) + """,
	"needsToDraw": """ + player.needsToDraw + """
}"""

	def makeImmutablePlayer(json: Option[Any]): ImmutablePlayer = {
		var supply = new CardSet
		var discard = new CardSet
		var field = new CardSet
		var hand = new CardSet
		var deck = new CardSet
		var actions = 0
		var buys = 0
		var coin = 0
		var topOfDeck = List[Card]()
		var needsToDraw = 0

		json match {
			case Some(m: Map[String, Any]) =>
				m("player") match {
					case p: Map[String, Any] =>
						supply = CardSet.makeCardSet(p("supply")) // Parameters of type Any.
						discard = CardSet.makeCardSet(p("discard"))
						field = CardSet.makeCardSet(p("field"))
						hand = CardSet.makeCardSet(p("hand"))
						deck = CardSet.makeCardSet(p("deck"))

						actions = p("actions").asInstanceOf[Double].toInt
						buys = p("buys").asInstanceOf[Double].toInt
						coin = p("coin").asInstanceOf[Double].toInt
						needsToDraw = p("needsToDraw").asInstanceOf[Double].toInt

						topOfDeck = p("topOfDeck").asInstanceOf[List[String]].map(CardSet.makeCard)

					case _ => throw new BadJSONException()
				}
			case _ => throw new BadJSONException()
		}
		return new ImmutablePlayer(supply, discard, field, hand, deck, actions, buys, coin, topOfDeck, needsToDraw)			
	}
}