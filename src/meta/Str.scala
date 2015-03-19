package meta

import parser.{ Parser,PString }
import data._

case class Str(x: String, name: String) extends Construct {
	def pars(already: List[(Group,Parser[Individual])] = List()) = PString(x) #> { case a => Sign(a,name) }
}