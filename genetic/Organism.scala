package dominion.src


class Organism(val decisionMatrix: DecisionMatrix, val valueDistribution: ValueDistribution) {

	var tourneysPlayed: Int = 0
	var totalWinPercentage: Double = 0

	def clearData {
		tourneysPlayed = 0
		totalWinPercentage = 0
	}

	def fitness: Double = {
		if (tourneysPlayed == 0) 0.0
		else totalWinPercentage / tourneysPlayed.toDouble
	}

	def addWinPercentage(percentage: Double) = {
		tourneysPlayed += 1
		totalWinPercentage += percentage
	}

	override def toString: String =
		"Value Distribution: " + valueDistribution + 
	    "\nDecision Matrix: " + decisionMatrix

	def mutate(shift: Double): Organism = {
		val rand = math.random

		// Doesn't mutate everything necessarily.
		if (rand <= 0.33) {
			new Organism(decisionMatrix.mutate(shift), valueDistribution.mutate(shift))
		} else if (rand <= 0.66) {
			new Organism(decisionMatrix.mutate(shift), valueDistribution)	
		} else {
			new Organism(decisionMatrix, valueDistribution.mutate(shift))	
		}
	}

	def toJSON: String = {
		"{ \"distribution\":" + valueDistribution.toJSON + ",\n \"matrix\":" + decisionMatrix.toJSON + " }" 
	}
}

object Organism {
	def randomOrganism: Organism = {
		new Organism(DecisionMatrix.randomMatrix, ValueDistribution.randomDistribution)
	}
}