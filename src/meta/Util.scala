package meta

import parser.{ Parser, Simple, PString }
import parser.AdditionalParsers._
import data._
import data.matchers._

object Util {
	var all = Group("all")

	val defaultStructs = List((all, None), (Num, None), (Numvar, Some("all")), (Var, Some("all")), (PatBind, Some("all")))

	case object Num extends Construct {
		def pars(already: List[(Group, Parser[Individual])] = List()) = Simple("""[0-9]+(\.[0-9]+)?""") #> { case a => Number(a.toDouble) }

		def name = "number"
	}

	case object Var extends Construct {
		def pars(already: List[(Group, Parser[Individual])] = List()) = Simple("""[A-Za-z]+""") #> { case a => Sign(a) }

		def name = "var"
	}

	case object Numvar extends Construct {
		def pars(already: List[(Group, Parser[Individual])] = List()) = (PString("num[") & Simple("""[A-Za-z]+""") & PString("]")) #> {
			case (_, a, _) => new ConditionedVar(a) {
				def condition(x: Individual) = x match {
					case Number(_) => true
					case _ => false
				}
			}
		}

		def name = "numberPattern"
	}

	all |+ Numvar

	case object PatBind extends Construct {
		def pars(already: List[(Group, Parser[Individual])] = List()) =
			(Simple("""[A-Za-z]+""") & PString("@") & all.pars(already)) #> {
				case (v, _, c) => new PatternBind(new Sign(v), c)
			}

		def name = "patternBind"
	}

	all |+ PatBind
}