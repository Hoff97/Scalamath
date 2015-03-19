package meta

import parser._
import data._

case class IgnoreString(str: String) extends Construct {
	def name = "ignore"

	def pars(already: List[(Group, Parser[Individual])]) = PString(str.slice(1, str.length - 1)) #> { case a => Sign(a, "ignore") }
}