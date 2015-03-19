package math

import data._
import util.ListUtil._
import math.StandardStructs._

trait Rule {
	def apply(x: Individual): Option[Individual]

	def applyAt(x: Individual): Option[Individual] = this.apply(x) match {
		case g @ Some(_) => g
		case _ => x match {
			case Struct(subs, tpe) => {
				(subs.zipWithIndex.mapWhile { case (a, b) => (applyAt(a), b) } {
					case (_, (a, _)) => !a.isDefined
				}).last match {
					case (Some(a), i) => Some(Struct(subs.updated(i, a), tpe))
					case _ => None
				}
			}
			case _ => None
		}
	}
}

case class StructedRule(s1: Individual, s2: Individual) extends Rule {
	def apply(x: Individual) = s2.matchWith(x) match {
		case Some(_) => None
		case None => s1.matchWith(x).map(a => s2.bindAll(a))
	}

	override def toString = s1.str + "=" + s2.str
}

case class NumberRule(Operand: String, f: (Double, Double) => Individual) extends Rule {
	def apply(x: Individual) = x match {
		case Struct(List(Number(a), Sign(Operand, _), Number(b)), "operation") => Some(f(a, b))
		case _ => None
	}
}

case class NumberFunc1(Func: String, f: Double => Individual) extends Rule {
	def apply(x: Individual) = x match {
		case Struct(List(Sign(Func, _), _, Number(d), _), "functionOne") => Some(f(d))
		case _ => None
	}
}

case class NumberFunc2(Func: String, f: (Double, Double) => Individual) extends Rule {
	def apply(x: Individual) = x match {
		case Struct(List(Sign(Func, _), _, Number(a), _, Number(b), _), "functionOne") => Some(f(a, b))
		case _ => None
	}
}

object Rule {
	val standardRules = List(NumberRule("+", { case (a, b) => Number(a + b) }),
		NumberRule("*", { case (a, b) => Number(a * b) }),
		NumberRule("/", { case (a, b) => Number(a / b) }),
		NumberRule("-", { case (a, b) => Number(a - b) }),
		NumberRule("<", { case (a, b) => bool(a < b) }),
		NumberRule("<=", { case (a, b) => bool(a <= b) }),
		NumberRule(">", { case (a, b) => bool(a > b) }),
		NumberRule(">=", { case (a, b) => bool(a >= b) }),
		NumberRule("^", { case (a, b) => Number(Math.pow(a, b)) }),
		NumberFunc1("sin", x => Number(Math.sin(x))),
		NumberFunc1("cos", x => Number(Math.cos(x))),
		NumberFunc1("tan", x => Number(Math.tan(x))))
}