package search

class EmptyTreeNode[T] extends TreeNode[T](???,???) {
	override def parentNode = this
	
	val value = ???
	
	override def toTop: List[TreeNode[T]] = List()
}