package meta

import parser._
import data._

case class Link(name: String) extends Construct {
	def pars(already: List[(Group, Parser[Individual])] = List()) = new Parser[Individual]() {
		def apply(str: String, depth: Int = 0) = throw new Error("Cant parse Link " + name)
		def minLength = throw new Error("Cant parse Link " + name)
		def maxLength = throw new Error("Cant parse Link " + name)
		val accelerated = false
		def possibleCuts(str: String) = Stream()
	}
}