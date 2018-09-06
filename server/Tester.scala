package dominion.src

import scala.util.parsing.json._
import scala.collection.immutable.HashMap

object Tester {
	def main(args: Array[String]) {
		
		println(CardSet.makeCard("Copper").cost)
		println(CardSet.makeCard("Remodel").cost)
		println(CardSet.makeCard("Province").cost)
		println(CardSet.makeCard("Workshop").cost)
		println(CardSet.makeCard("Feast").cost)

		
		// Case classes get equality for free.
		val a = new Copper
		val b = new Copper
		val c = new Workshop(None)
		val d = new Workshop(None)
		val e = new Workshop(Some(a))
		val f = new Workshop(Some(a))
		val g = new Workshop(Some(b))

		println(a == b)
		println(c == d)
		println(e == f)
		println(e == g)
		println(c != e)


		val supply = new CardSet(HashMap[Card, Int](
						new Remodel(None, None) -> 10,
						new Mine(None, None) -> 10,
						new Workshop(None) -> 10,
						new Militia -> 10,
						new Woodcutter -> 10,
						new Estate -> 50,
						new Copper -> 50,
						new Silver -> 50,
						new Gold -> 50))


		val deck = new CardSet(HashMap[Card, Int](
					//new Remodel(None, None) -> 1,
					new Mine(None, None) -> 1,
					new Militia() -> 1,
					new Woodcutter() -> 1,
					new Copper -> 5,
					new Estate -> 3))

		val discard = new CardSet(HashMap[Card, Int]())
		val field = new CardSet(HashMap[Card, Int]())

		val hand = new CardSet(HashMap[Card, Int](
					new Smithy() -> 1,
					//new Remodel(None, None) -> 1,
					//new Mine(None, None) -> 1,
					new Militia() -> 2,
					new Woodcutter() -> 2,
					new Copper -> 1))

		val testAnother = new CardSet(HashMap[Card, Int](
					new Smithy() -> 1,
					//new Remodel(None, None) -> 1,
					//new Mine(None, None) -> 1,
					new Militia() -> 2,
					new Woodcutter() -> 2))



		// Checks out.
		println(HandGenerator.allPossibleBuys(hand, 10, 2))

		//println((new CardSet(testAnother)).toEnglish)
		//println((new CardSet(testAnother)).toJSON)




		val testJSON2 = """
		{
			"id": "playRound",
			"player": {
				"supply": {
					"Copper": 1,
					"Curse": 5
				},
				"discard": {
					"Copper": 2
				},
				"field": {
					"Copper": 3
				},
				"hand": {
					"Festival": 1,
					"Gold": 1,
					"Market": 1,
					"Smithy": 1,
					"Estate": 1
				},
				"deck": {
					"Copper": 5
				},
				"actions": 1,
				"buys": 1,
				"coin": 0,
				"topOfDeck": [
					"Copper",
					"Silver"
				],
				"needsToDraw": 0
			}
		}
		"""


		val testJSON = """
		{
  "id" : "playRound",
  "player" : {
    "supply" : {
      "Witch" : 9,
      "Village" : 10,
      "Gold" : 50,
      "Laboratory" : 10,
      "Copper" : 43,
      "Duchy" : 50,
      "Smithy" : 9,
      "Remodel" : 10,
      "Estate" : 47,
      "Council Room" : 10,
      "Cellar" : 9,
      "Chancellor" : 10,
      "Chapel" : 10,
      "Curse" : 29,
      "Moat" : 10,
      "Feast" : 10,
      "Woodcutter" : 10,
      "Festival" : 10,
      "Province" : 12,
      "Bureaucrat" : 10,
      "Silver" : 45,
      "Workshop" : 10
    },
    "topOfDeck" : [

    ],
    "id" : "playRound",
    "hand" : {
      "Copper" : 1,
      "Curse" : 1,
      "Estate" : 3
    },
    "deck" : {
      "Copper" : 4,
      "Smithy" : 1,
      "Cellar" : 1
    },
    "coin" : 0,
    "actions" : 1,
    "buys" : 1,
    "field" : {

    },
    "needsToDraw" : 0,
    "discard" : {
      "Copper" : 2,
      "Witch" : 1,
      "Silver" : 5
    }
  }
}
"""

		//println(ValueDistribution.randomDistribution)
		//println(ValueDistribution.randomDistribution)
		//println(ValueDistribution.randomDistribution)
		//println(ValueDistribution.randomDistribution)

		// val bestHand = new CardSet(HashMap[Card, Int](
		// 		new Copper -> 3,
		// 		new 
		// 		new Silver -> 7
		// 	))
		// val bestHand2 = new CardSet(HashMap[Card, Int](
		// 		new Copper -> 2,
		// 		new Silver -> 7
		// 	))
		
//		val player = new ImmutablePlayer(supply, discard, field, hand, deck, 1, 1, 0, List[Card](), 0)
//		val player = new ImmutablePlayer(new CardSet, new CardSet, new CardSet, bestHand, new CardSet, 1, 1, 0, List[Card](), 0)
//		val player2 = new ImmutablePlayer(new CardSet, new CardSet, new CardSet, bestHand2, new CardSet, 1, 1, 0, List[Card](), 0)
//		val distr = new ValueDistribution(HashMap[Card, Double](
			// 	new Copper -> 0.3,
			// 	new Silver -> 0.7
			// ), 2)
		//val rand1 = ValueDistribution.randomDistribution
		//val rand2 = ValueDistribution.randomDistribution
		///println("\n\n\nDistribution: " + distr)
		//println("\n\n\nPlayer: " + player)
		//println("Distance:" + rand1.distanceTo(rand2))
		// println("\n\n\nValue of player: " + distr.asValueFunction(player))
		// println("\n\n\nValue of player2: " + distr.asValueFunction(player2))




		//val br = new BestValueRetriever
		//println(br.matricesAndDistributions.size)
		val br = new BestValueRetriever
		val sh = new SuggestionHandler(testJSON, br)
		//println("\n\n\nmakeImmutablePlayer")
		val player = sh.makeImmutablePlayer(JSON.parseFull(testJSON))
		val allPlayerCards = player.allCards
		val withCurse = allPlayerCards.addOne(new Curse)
		val distr = br.getBest(player.supply)._2

		//val allCardsOne = new CardSet(HashMap[Card, Int](new Smithy -> 1, new Cellar -> 1, new Silver -> 5, new Copper -> 7, new Curse -> 1, new Witch -> 1, new Estate -> 3))
		//val allCardsNo = new CardSet(HashMap[Card, Int](new Smithy -> 1, new Cellar -> 1, new Silver -> 5, new Copper -> 7, new Witch -> 1, new Estate -> 3))


		val allCardsNo = new CardSet(HashMap[Card, Int](
				new Silver -> 1,
				new Province -> 2
			))
		val allCardsOne = new CardSet(HashMap[Card, Int](
				new Silver -> 10,
				new Curse -> 1,
				new Province -> 20
			))

		println("No curse: " + allCardsNo.toValueDistribution)
		println("One curse: " + allCardsOne.toValueDistribution)

		println("All cards: " + allCardsNo)
		println("All with curse: " + allCardsOne)
		println("Distribution: " + distr)
		println("Without curse: " + distr.distanceTo(allCardsNo.toValueDistribution))
		println("With curse: " + distr.distanceTo(allCardsOne.toValueDistribution))

		//println("Without curse: " + br.getBest(player.supply)._2.asValueFunction(player))
		//println("With curse: " + br.getBest(player.supply)._2.asValueFunction(player.addToDiscard(new Curse)))

		println("\n\n\nmakeResponse")
		println(sh.makeResponse)
		

		//val pop = new Population
		//pop.writeToJSON(pop.population)

		/*
		val actions = 1
		val buys = 1
		val coin = 0

		for (a <- 0 to 16) {
			for (b <- 0 to a) {
				print(HandGenerator.choose(a,b).toInt + " ")
			}
			println
		}


		val immutablePlayer = new ImmutablePlayer(supply, discard, field, hand, deck, actions, buys, coin, List[ImmutableOpponent]())
		var totalProb: Double = 0
		for (h <- HandGenerator.generateHands(immutablePlayer, 3)) {
			totalProb += h._2
			println(h._1.hand)
		}

		val p2 = new ImmutablePlayer(supply, testAnother, field, testAnother, discard, actions, buys, coin, List[ImmutableOpponent]())
	 	totalProb = 0
		for (h <- HandGenerator.generateHands(p2, 3)) {
			totalProb += h._2
			println(h._1.hand + ", " + h._2)
		}

		println("Total probability: " + totalProb)
		println
		var tot = 0.0
		for (comb <- HandGenerator.combinationsOfNumber(hand, 4)) {
			println(comb._1)
			tot += comb._2
		}
		println("Total prob: " + tot)
		println
		tot = 0
		for (comb <- HandGenerator.combinationsOfNumber(deck, 11)) {
			println(comb._1)
			tot += comb._2
		}
		println("Total prob: " + tot)
		println
		tot = 0
		for (comb <- HandGenerator.combinationsOfNumber(testAnother, 3)) {
			println(comb._1)
			tot += comb._2
		}
		println("Total prob: " + tot)



		val player = new ImmutablePlayer(supply, discard, field, hand, deck, actions, buys, coin, List[ImmutableOpponent]())
		val player2 = new ImmutablePlayer(supply, field, hand, deck, discard, actions, buys, coin, List[ImmutableOpponent]())
	*/}
}