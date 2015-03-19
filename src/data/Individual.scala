package data

import math.Binding

//TODO Better type specifications
trait Individual {
	def str: String

	def matchWith(x: Individual): Option[List[Binding]]

	def bind(x: Binding): Individual
	def bindAll(x: List[Binding]): Individual

	def tpe: String
}

object Individual {
	implicit class Operations(x: Individual) {
		def +(y: Individual) = Struct(List(x, Sign("+", "str"), y), "operation")
		def -(y: Individual) = Struct(List(x, Sign("-", "str"), y), "operation")
		def *(y: Individual) = Struct(List(x, Sign("*", "str"), y), "operation")
		def /(y: Individual) = Struct(List(x, Sign("/", "str"), y), "operation")

		def f(name: String) = Struct(List(Var(name), Sign("("), x, Sign(")")), "functionApp")
	}

	implicit class Functions(x: Struct) {
		def apply(y: Individual) =
			if (x.tpe == "function")
				FunctionApp(x.subs(0), y)
	}

	def Var(x: String) = Sign(x, "var")
	def Function(x: String) = Struct(List(Var(x)), "function")
	def FunctionApp(x: Individual, y: Individual) = Struct(List(x, Sign("("), y, Sign(")")), "functionApp")
}