package search

trait Graph[T,N <: Node[T]] {
	def connected(n: N): List[N]
}