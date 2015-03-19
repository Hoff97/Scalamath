package search

class AStarEmptyNode[T] extends AStarNode[T](null.asInstanceOf[T],null.asInstanceOf[AStarNode[T]],0.0) {
	override def toTop: List[TreeNode[T]] = List()
	
	override def sumH: Double = 0.0
}