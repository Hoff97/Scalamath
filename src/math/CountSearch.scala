package math

import search.AStar
import data._
import search.DepthFirst
import search.Tree
import search.TreeNode

class CountSearch(x: Individual => Boolean, rules: List[Rule]) extends AStar[Individual] {
	def heuristic(a: Individual) = Util.count(a, x)

	def next(a: Individual) = {
		val ruleApp = rules.map(_.applyAt(a).flatMap(Util.simplify(_))).foldLeft(List[Individual]()) {
			case (a, Some(b)) => { println(b.str); b :: a }
			case (a, None) => a
		}
		Util.simplify(a) match {
			case Some(x) => x :: ruleApp
			case _ => ruleApp
		}
	}

	override def difference(t1: Individual, t2: Individual) = 3
}