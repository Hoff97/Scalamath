package math

import data._
import util.ListUtil._
import meta.Language

object Util {
	def simplify(x: Individual): Option[Individual] = x match {
		case Struct(a, b) => simplify(a, b)
		case _ => Some(x)
	}

	def simplify(list: List[Individual], b: String): Option[Individual] = b match {
		case "application" => list(0) match { //TODO Rule Application on lower levels
			case g @ Struct(_, "rule") => applyRule(g, list(2))
			case _ => simplify(list(0)).flatMap(x => simplify(list(2)).map(y => y.bindAll(bindList(x))))
		}
		case "matching" => list(0).matchWith(list(2)).map(x => constructBindList(x))
		case _ => Some(Struct(list, b))
	}

	def constructBindList(x: List[Binding]): Individual = x match {
		case a :: b :: Nil => Struct(List(a, Sign(",", "k"), b), "bindlist")
		case a :: Nil => Struct(List(a), "bindlist")
		case a :: b => Struct(List(a, Sign(",", "k"), constructBindList(b)), "bindlist")
		case _ => Struct(List(), "bindlist")
	}

	def bindList(x: Individual): List[Binding] = x match {
		case Struct(l, a) if a == "bindlist" || a == "list" =>
			if (l.length == 0)
				Nil
			else if (l.length == 1)
				bind(l(0)) :: Nil
			else
				bind(l(0)) :: bindList(l(2))
		case Struct(l, "binding") => List(bind(x))
	}

	def applyRule(x: Struct, y: Individual): Option[Individual] = x match {
		case Struct(List(a, _, b), "rule") => a.matchWith(y).map(b.bindAll(_)) match {
			case None => y match {
				case Struct(subs, tpe) => {
					(subs.zipWithIndex.mapWhile { case (a, b) => (applyRule(x, a), b) } {
						case (_, (a, _)) => !a.isDefined
					}).last match {
						case (Some(a), i) => Some(Struct(subs.updated(i, a), tpe))
						case _ => None
					}
				}
				case _ => None
			}
			case g @ Some(_) => g
		}
	}

	def bind(x: Individual) = x match {
		case Struct(l, "binding") => new Binding(l(0), l(2))
	}

	def count(a: Individual, x: Individual => Boolean): Int = (if (x(a)) 1 else 0) + (a match {
		case Struct(subs, _) => subs.foldLeft(0) { case (c, d) => c + count(d, x) }
		case _ => 0
	})

	def fromFile(str: String) = {
		val f = scala.io.Source.fromFile(str).mkString.split("\r\n")
		val lang = Language.fromFile(f.head.split(" ")(1))
		val rules = (f.tail.filter(_.length > 0).map(x => (x, lang.parse(x))).foldLeft(List[Rule]()) {
			case (a, (_, (b @ Struct(List(s1, _, s2), "rule")) :: _)) => StructedRule(s1, s2) :: a
			case (a, (b, Nil)) => {
				println("Couldnt parse " + b)
				a
			}
		}) ::: Rule.standardRules
		(lang, rules)
	}
}