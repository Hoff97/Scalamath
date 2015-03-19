package search

abstract class TreeNode[+T](v: T, parent: TreeNode[T]) extends Node[T] {
	def parentNode = parent

	def toTop: List[TreeNode[T]] = this :: parent.toTop
}

object TreeNode {
	def startNode[T](t: T) = new SomeTreeNode[T](t, new EmptyTreeNode)
}