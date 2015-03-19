package meta

import parser.{ Parser,Simple }
import data._

case class RE(x: String, name: String) extends Construct {
	def pars(already: List[(Group,Parser[Individual])] = List()) = Simple(x) #> { case a => Sign(a,name) }
}