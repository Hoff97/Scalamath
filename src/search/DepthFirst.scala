package search

abstract class DepthFirst[T] extends TreeSearch[T, SomeTreeNode[T]] {
	def sort(x: SomeTreeNode[T]) = 0.0

	def startKnot(x: T) = SomeTreeNode.startNode(x)

	val tree = new Tree[T, SomeTreeNode[T]] {
		def connected(n: SomeTreeNode[T]) = next(n.value).map(x => SomeTreeNode(x, n))
	}

	def next(n: T): List[T]
}