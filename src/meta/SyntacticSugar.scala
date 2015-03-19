package meta

import parser._
import data._

case class SyntacticSugar(parser: CList, redefinition: Individual, name: String) extends Construct {
	def pars(already: List[(Group, Parser[Individual])] = List()) = parser.pars(already) #> {
		case x => replaceLinksData(redefinition, x.subs.zipWithIndex.map {
			case (a, b) => (b + "", a)
		})
	}

	def replaceLinksData(x: Individual, a: List[(String, Individual)]): Individual = x match {
		case Sign(b, "link") => a.find(_._1 == b).map(_._2).getOrElse(x)
		case Struct(subs, n) => Struct(subs.map(replaceLinksData(_, a)), n)
		case _ => x
	}
}