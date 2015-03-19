package meta

import data._
import parser._

case class CList(subs: List[Construct], name: String) extends Construct {
	def pars(already: List[(Group, Parser[Individual])] = List()): Parser[Struct] = (subs.foldLeft(ListP[Individual](List())) {
		case (a, b) => (a ++ b.pars(already))
	}) #> { case x => Struct(x, name) }
}