package dominion.src

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.net.ServerSocket
import java.net.URLDecoder
import scala.collection.mutable.HashMap

/**
  * Runs the server.
  */
class Server {
	
	val port = 8080
	val validKeys = List[String]("5LgM9Spab8ybGyBUF3eRVKQhazD7QGuS5skccDgqfcf5fNgpd") // 62^49 combinations

	val bestRetriever = new BestValueRetriever // Do this once at the start.
	// Might need extra memory because of this.

	/**
	  * Runs the server, taking requests indefinitely.
	  */
	def run {
		println("Server started.")
		val serverSocket = new ServerSocket(port)
		
		// Want the server to continue running indefinitely.
		while(true) {
			val socket = serverSocket.accept
			println("Connected to client.")

			val in = new BufferedReader(
							new InputStreamReader(socket.getInputStream))
			val request = readInput(in)
			socket.shutdownInput
			println("Read request.")

			// Where all the fun stuff happens.
			val response = handleRequestWithErrors(request) 
			println("Handled request.")
			//println("Response:")
			//println(response)

			val w = new BufferedWriter(
							new OutputStreamWriter(socket.getOutputStream))
			w.write(response)
			w.flush
			socket.shutdownOutput
			socket.close
			println("Sent response.")
		}
	}

	/**
	  * Reads all the input from the input reader and returns it as a string.
	  *
	  * @param reader - the input BufferedReader
	  * @return - the reader's entire contents
	  */
	def readInput(reader: BufferedReader): String = {
		var input = ""
		var current = reader.read
		val endCharacters = List[Int](13, 10, 13, 10)
		var lastFour = List[Int](0, 0, 0, 0)

		while(current != -1 || lastFour == endCharacters) { // Reads next character.
			lastFour = lastFour.tail ::: List(current)
			input += current.toChar
			if (lastFour == endCharacters) return input // Equivalent to break.
			current = reader.read
		}

		input
	}

	/**
	  * Wrapper for the handle request method.
	  * Catches all the errors that could be thrown by
	  * handleRequest(), and returns an error message with error html
	  * if an exception is caught. Otherwise returns the server's response.
	  *
	  * @param request - the request as a string
	  * @return - the response as a string, could be an error message
	  */
	def handleRequestWithErrors(request: String): String = {

		try {
			println("Request:")
			println(request)
			handleRequest(request)
		} catch {
			case e: BadJSONException =>
				"{\"error\": \"bad json\"}"
			case e: BadRequestException =>
				"{\"error\": \"bad requests\"}"
			case e: InvalidKeyException =>
				"{\"error\": \"invalid key\"}"
			case e: Exception =>
				println(e)
				"{\"error\": \"internal error\"}"
		}
	}

	/**
	  * Returns the response.
	  * Handles errors too, throws an error if something went wrong.
	  * Parses the request and gets the corresponding page, with
	  * a new session ID, and updates the sessionID map.
	  * 
	  * @param request - the request as a string
	  * @return - the response as a string, won't be error message.
	  */
	def handleRequest(request: String): String = {
		
		// Could probably end up with a lot less code using regex.
		val parts = request.split("\r\n")
		if (parts.length >= 2) {
			val path = parts(0)
			val key = parts(1)
			if (validKeys.contains(key)) {
				val pathParts = path.split(" ")
				if (pathParts.length == 2) {
					if (pathParts(0) == "DOMINION") {
						if (pathParts(1) != "SUGGESTION") {
							return handleSimpleRequest(pathParts(1))
						} else {
							if (parts.length == 3) {
								return handleSuggestion(parts(2))
							}
						}
					} 
				} 
				throw new BadRequestException // If we haven't returned yet, throw this exception.
			} else {
				throw new InvalidKeyException
			}
		} else {
			throw new BadRequestException
		}
		return "" // Will not get here.
	}

	def handleSuggestion(playerJSON: String): String = {
		val handler = new SuggestionHandler(playerJSON, bestRetriever)
		return handler.makeResponse
	}

	def handleSimpleRequest(requestText: String): String = requestText match {
		case "MENUOPTIONS" => return menuOptions
		case "SUPPLYOPTIONS" => return supplyOptions
		case "DISCLAIMER" => return disclaimer
		case _ => 
			throw new BadRequestException
			return ""
	}

	val menuOptions: String =
		"""
		{
			"playRound": "Play Round",
			"removeFromSupply": "Remove cards from the Supply",
			"militia": "Get attacked by Militia",
			"witch": "Get attacked by Witch",
			"bureaucrat": "Get attacked by Bureaucrat",
			"councilRoom": "Opponent played Council Room"
		} 
		"""

	val supplyOptions: String = 
		"""
		{
			"supported": [
				"Copper",
				"Silver",
				"Gold",
				"Estate",
				"Duchy",
				"Province",
				"Village",
				"Festival",
				"Laboratory",
				"Market",
				"Smithy",
				"Moat",
				"Woodcutter",
				"Council Room",
				"Witch",
				"Remodel",
				"Cellar",
				"Chancellor",
				"Bureaucrat",
				"Chapel",
				"Mine",
				"Workshop",
				"Feast",
				"Moneylender",
				"Militia",
				"Curse"
			]
		}
		"""

	val disclaimerText = "Dominion Assistant only supports the base set of cards. Currently no other set is implemented. From the base set, " +
					 	 "the following cards have not been implemented: Spy, Theif, Gardens, Throne Room, Library, Adventurer. If you are playing a game " +
					 	 "with any of those cards, you will not be able to use the assistant." +  
						 " ~Furthermore, this AI is not perfect and is unaware of the opponents actions. The AI was trained by playing against variations of itself in two player games." +
						 "Performance may vary in games with more than two people. ~The default number of curses in the supply is 30. If " +
						 "your curses ran out in the game, but there are still curses in the app, avoid tapping 'get attacked by witch', in order " +
						 "to prevent the AI from thinking you gained a curse."

	val disclaimer = 
		"""
		{
			"disclaimer": " """ + disclaimerText + """ "
		}
		"""

}


/**
  * The static object of the Server.
  */
object Server {

	/**
	  * Makes a server and runs it.
	  *
	  * @param args - the program's arguments, ignored here
	  */
	def main(args: Array[String]) { 
		val server = new Server
		println("Running server.")
		server.run
	}
}
