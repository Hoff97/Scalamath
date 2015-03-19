package meta

import parser.Parser
import data.Individual

case class PatApp(n: String, args: List[Construct], name: String = "app") extends Construct {
	def pars(already: List[(Group, Parser[Individual])] = List()) = throw new Error("Cant get Parser of Application " + name)
}