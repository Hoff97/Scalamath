package search

case class SomeTreeNode[+T](value: T, parent: TreeNode[T]) extends TreeNode[T](value, parent)

object SomeTreeNode {
	def startNode[T](x: T) = new SomeTreeNode(x, null) {
		override def toTop: List[TreeNode[T]] = this :: Nil
	}
}