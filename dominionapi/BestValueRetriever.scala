package dominion.src // This really should have been changed.

import scala.collection.mutable.ListBuffer
import scala.util.parsing.json._
import scala.collection.immutable.HashMap
import scala.io.Source


class BestValueRetriever { // Really only need supply.

	// Ordering matters!
	val matricesAndDistributions = new ListBuffer[(DecisionMatrix, ValueDistribution)]()

	// Parse JSON here.
	var aggregate = ""
	var numberOfUnfinishedLeftBrackets = 0
	var pastFirstTimeNumberIsOne = false

	// Idea: Parse json into mini jsons, so that we don't manage a string
	// of 15000 lines long.
	for(line <- Source.fromFile("best.json").getLines) {
		
		if (aggregate == "{" && line.head == ',') {
			aggregate += line.tail
		} else {
			aggregate += line
		}

		val bracketdiff = line.count(_ == '{') - line.count(_ == '}')
		numberOfUnfinishedLeftBrackets += bracketdiff
		
		if (numberOfUnfinishedLeftBrackets == 1 && bracketdiff != 0) {
			if (pastFirstTimeNumberIsOne) {
				println("Parsing section...")
				matricesAndDistributions += parseSection(aggregate + "}")
				aggregate = "{"
				numberOfUnfinishedLeftBrackets = 1
				// keep pastFirstTimeNumberIsOne as true.
			} else {
				pastFirstTimeNumberIsOne = true
			}
		}
	}

	println(matricesAndDistributions.size)

	// Finish this.
	private def parseSection(jsonString: String): (DecisionMatrix, ValueDistribution) = {
		val json: Option[Any] = JSON.parseFull(jsonString)
		var matrix: Option[DecisionMatrix] = None
		var distr: Option[ValueDistribution] = None

		json match {

			case Some(m: Map[String, Any]) =>

				m.head._2 match {
					case n: Map[String, Any] => 

						n("matrix") match {
							case matr: Map[String, Any] =>
								matrix = Some(DecisionMatrix.parseJSON(matr))
							case _ => throw new BadJSONException
						}

						n("distribution") match {
							case dis: Map[String, Double] =>
								val data = makeCorrectHashMap(dis)
								distr = Some(new ValueDistribution(data._1, data._2))
							case _ => throw new BadJSONException
						}

					case _ => 
						throw new BadJSONException	
				}

			case _ => 
				throw new BadJSONException
		}

		return (matrix.get, distr.get) // Will have already thrown error either values don't exist.
	}

	private def makeCorrectHashMap(m: Map[String, Double]): (HashMap[Card, Double], Int) = {
		var hash = HashMap[Card, Double]()
		var focusCards = 0
		for ((card, count) <- m) {
			hash += ((CardSet.makeCard(card), count))
			focusCards += 1
		}
		return (hash, focusCards)
	}

	def getBest(supply: CardSet): (DecisionMatrix, ValueDistribution) = {
		for (matrixAndDistribution <- matricesAndDistributions) {
			if (matchWell(supply, matrixAndDistribution._2)) {
				return matrixAndDistribution
			}
		}

		print("No good distribution found.")

		if (matricesAndDistributions.size == 0) throw new Exception("BestValueRetriever has no values.")


		return matricesAndDistributions.head // Will throw error if the list buffer is empty.
	}

	// Checks if the distribution could be carried out under the given supply.
	private def matchWell(supply: CardSet, distr: ValueDistribution): Boolean = {
		var total = 0
		var matched = 0

		for ((card, count) <- distr.map) {
			if (count > 0.15) {
				total += 1
				if (supply.contains(card)) { 
					matched += 1
				}
			}
		}

		return (total == matched && total > 0)
	}
}