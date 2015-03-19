package search

case class AStarNode[T](value: T, parent: AStarNode[T], heuristic: Double) extends TreeNode[T](value,parent) {
	def sumH: Double = parent.sumH+heuristic
}