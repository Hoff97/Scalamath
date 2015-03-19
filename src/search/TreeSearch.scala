package search

import scala.annotation.tailrec

trait TreeSearch[T, N <: TreeNode[T]] {
	val tree: Tree[T, N]

	def aim(t: T): Boolean

	def sort(x: N): Double

	def startKnot(x: T): N

	@tailrec
	final def search(seen: List[N], known: List[N], level: Int = 0): List[T] = {
		if (level > TreeSearch.maxlevel)
			List()
		else {
			val seenS = seen.sortBy(sort)
			val next = seenS.head
			if (aim(next.value)) {
				println(known.length)
				next.toTop.map(_.value)
			} else
				search(tree.connected(next).filter(x => !known.contains(x) && !seenS.contains(x)) ::: seenS.tail, next :: known, level + 1)
		}
	}

	def search(x: T): List[T] = search(List(startKnot(x)), Nil)
}

object TreeSearch {
	var maxlevel = 5000
}