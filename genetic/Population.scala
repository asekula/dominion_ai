package dominion.src

import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter


class Population {
	val size = 1000 // We want this to be even.
	val numberToContinue = 30 // Note: The size of this may result in needing more memory for the server. Keep this relatively low.
	val mutations = 19
	val randomExtra = size - (numberToContinue * (mutations + 1)) // Here it's 400.
	val gamesInTournament = 20
	val mutationShift = 0.05 // What to put here?

	val battlesPerGeneration = 30 // Number of other players it will encounter per generation.
	val generations = 100

	// Generate random organisms in the constructor.
	var population: List[Organism] = (1 to size).map(a => Organism.randomOrganism).toList
	
	def evolve {
		for (a <- 1 to generations) {
			makeNextGen
		}
		
		println("Final winner:")
		println(population.head)
	}

	def makeNextGen {
		for (a <- 1 to battlesPerGeneration) {
			population = util.Random.shuffle(population)
			val halfSize = (size / 2)
			for (i <- 0 until halfSize) {
				makeBattle(population(i), population(i + halfSize))
				loadingSmall(i, halfSize)
			}
			println("\nFinished battling round " + a + " of " + battlesPerGeneration + "\n")
		}

		population = population.sortBy(org => org.fitness * -1).take(numberToContinue) // Kill the weakest.
		println("Generation winner with fitness " + population.head.fitness + ": " + population.head) // Print winner.
		writeToJSON(population)
		population = mutate(population)
	}

	def makeBattle(org1: Organism, org2: Organism) {
		val game = new Game(org1, org2)
		val p1wins = game.playTournament(gamesInTournament)
		org1.addWinPercentage(p1wins)
		org2.addWinPercentage(1 - p1wins)
	}

	def mutate(pop: List[Organism]): List[Organism] = {
		val biggerPop = ListBuffer[Organism]()
		for (org <- pop) {
			org.clearData
			biggerPop += org
			for (i <- 1 to mutations) {
				biggerPop += org.mutate(mutationShift) // Generates random mutation.
			}
		}

		if (biggerPop.size != (mutations + 1) * numberToContinue) {
			throw new Exception("You did the math wrong.")
		}

		// Generates random.
		for (i <- (biggerPop.size + 1) to size) {
			biggerPop += Organism.randomOrganism
		}

		if (biggerPop.size != size) {
			throw new Exception("Next population is not the same size as the previous one.")
		}

		return biggerPop.toList
	}

	// For values less than 50, greater than (or equal to) 10.
	def loadingSmall(counter: Int, total: Int) {
		if (total < 10) return

		if (counter % (total / 10) == 0) {
			print(counter / (total/10))
		} else print("#")
	}

	// NOTE: If you want to make this faster, maybe pass in the bufferedWriter into the toJSON
	// functions, so that we don't write really long lines. Only if this is too slow.
	def writeToJSON(pop: List[Organism]) {
		println("Writing to file...")
		val file = new File("best.json")
		val fileWriter = new FileWriter(file)
		val buffer = new BufferedWriter(fileWriter)
		var currentIndex = 1
		
		buffer.write("{")
		buffer.newLine
		
		for (org <- pop) {
			if (currentIndex != 1) {
				buffer.write(",")
			}
			
			buffer.write("\"" + currentIndex + "\":")
			buffer.newLine
			buffer.write(org.toJSON)
			buffer.newLine

			currentIndex += 1
		}

		buffer.write("}")
		buffer.close
		fileWriter.close
	}

	def setStartingPopulation(orgs: List[Organism]) {
		population = mutate(orgs)
	}
}

object Population {
	def main(args: Array[String]) {

		println("It has begun.")
		val p = new Population
		
		if (args.length == 1) {
			if (args(0) == "-continue") {
				val bestRetriever = new BestValueRetriever
				val startingOrgs = bestRetriever.matricesAndDistributions.map(a => new Organism(a._1, a._2)).toList
				p.setStartingPopulation(startingOrgs)
			}
		}

		// Allow for args saying it should read from input best.json.

		println("Begun evolution.")
		p.evolve
	}
}