package math

import data._

object Counters {
	def functions(x: Individual) = x match {
		case Struct(_, a) => a == "functionOne" | a == "functionTwo"
		case _ => false
	}

	def atomic(x: Individual) = x match {
		case Sign(_, _) => false
		case Number(_) => false
		case _ => true
	}

	def function(n1: String)(x: Individual) = x match {
		case Struct(Sign(n2, _) :: l, a) => n1 == n2 && (a == "functionOne" || a == "functionTwo")
		case _ => false
	}

	def function(n1: List[String])(x: Individual) = x match {
		case Struct(Sign(n2, _) :: l, a) => n1.contains(n2) && (a == "functionOne" || a == "functionTwo")
		case _ => false
	}

	def multiple(c1: Individual => Boolean, c2: Individual => Boolean)(x: Individual) = c1(x) || c2(x)

	def awithoutB(c1: Individual => Boolean, c2: Individual => Boolean)(x: Individual) = c1(x) && !c2(x)

	implicit class CountHelp(x: Individual => Boolean) {
		def &(a: Individual => Boolean) = multiple(x, a) _
		def !(a: Individual => Boolean) = multiple(x, a) _
	}
}