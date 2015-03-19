package examples

import search._
import search.Tree

class DFTest extends DepthFirst[List[Int]] {
	def aim(t: List[Int]) = trans(t) == 1 && t.length > 0

	def trans(x: List[Int]) = x.zipWithIndex.foldLeft(0) {
		case (a, (b, c)) => a + b * c * 3
	}

	def next(x: List[Int]) =
		if (x.length > 3)
			Nil
		else
			List(x ::: List(0), x ::: List(1), x ::: List(2))
}