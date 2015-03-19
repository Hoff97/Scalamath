package math

import data._

trait Evalutator {
	def eval(x: Individual): Individual
}

object TestEval extends Evalutator {
	def eval(x: Individual) = x

	def ev(x: Individual, rules: List[Rule]) = {

	}

	def mergeSubs(x: Individual, rules: List[Individual]) = rules.flatMap(searchSubs(x, _)).groupBy(_._1).transform {
		case (_, a) => a.map(_._2)
	}

	def searchSubs(x: Individual, r: Individual) = x match {
		case Struct(subs1, tpe) => r match {
			case Struct(subs2, `tpe`) => subs1.zip(subs2).map(x => compare(x._1, x._2))
				.foldLeft(List[(Individual, Individual)]()) {
					case (a, b) => b match {
						case Some((v, m)) => m match {
							case Sign(n, t) if t == "var" || t == "ignore" => a
							case _ => (v, m) :: a
						}
						case None => a
					}
				}
			case _ => List()
		}
		case _ => List()
	}

	def compare(x: Individual, y: Individual) =
		if (x.equals(y))
			None
		else
			Some((x, y))
}