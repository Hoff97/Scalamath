package math

import data._

class Binding(x: Individual, y: Individual) extends Struct(List(x, Sign("->", "str"), y), "binding") {
	def fst = x
	def scnd = y
}