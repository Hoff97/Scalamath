package examples

import search.AStar
import util.Vector
import util.VectorUtil._

class GridSearch(aim: Vector, not: List[Vector]) extends AStar[Vector] {
	def heuristic(v: Vector) = (v - aim).length

	override def difference(t1: Vector, t2: Vector) = (t1 - t2).length

	def next(v: Vector) = List(1.0 ~ 0.0, -1.0 ~ 0.0, 0.0 ~ 1.0, 0.0 ~ (-1.0)).map(_ + v).filter(!not.contains(_))
}