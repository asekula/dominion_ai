package dominion.src

import scala.collection.mutable.HashMap // Mutable.
import scala.collection.mutable.ArrayBuffer

object Memoization {

	val saved = HashMap[(Int, Int), Int]()

	def choose(n: Int, k: Int): Int = {
		if (saved.contains((n,k))) saved(n,k)
		else {
			if (k == 0) {
				saved((n,k)) = 1
				return 1
			} else {
				if (k == n) {
					saved((n,k)) = 1
					return 1
				} else {
					val result = choose(n - 1, k - 1) + choose(n - 1, k)
					saved((n, k)) = result
					return result
				}
			}
		}
	}
}