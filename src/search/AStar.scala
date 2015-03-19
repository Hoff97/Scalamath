package search

abstract class AStar[T] extends TreeSearch[T, AStarNode[T]] {
	def heuristic(t: T): Double

	def next(n: T): List[T]

	def aim(t: T) = heuristic(t) == 0

	def difference(t1: T, t2: T) = Math.abs(heuristic(t2) - heuristic(t1))

	def sort(n: AStarNode[T]) = n.sumH + heuristic(n.value)

	val tree = new Tree[T, AStarNode[T]] {
		def connected(n: AStarNode[T]) = next(n.value).map(x => AStarNode(x, n, difference(n.value, x)))
	}

	def startKnot(x: T) = AStar.startNode(x)
}

object AStar {
	def startNode[T](t: T) = new AStarNode(t, new AStarEmptyNode[T], 0.0)
}