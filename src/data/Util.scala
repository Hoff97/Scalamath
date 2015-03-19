package data

import math.Binding

object Util {
	def consistent(x: List[Binding]) = x.groupBy(_.fst).map(_._2).forall(x => x.forall(y => x.forall(_==y)))
}