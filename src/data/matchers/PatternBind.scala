package data.matchers

import data._
import math.Binding

class PatternBind(variable: Sign, c: Individual) extends Individual {
	def str = variable.str + "@" + c.str

	def matchWith(x: Individual) = c.matchWith(x).map(a => new Binding(variable, x) :: a)

	def bind(x: Binding) = c.bind(x)
	def bindAll(x: List[Binding]) = c.bindAll(x)

	def tpe = "patternBind"
}