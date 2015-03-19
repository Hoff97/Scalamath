package search

trait Tree[T,N <: TreeNode[T]] extends Graph[T,N] {
	override def connected(n: N): List[N]
}