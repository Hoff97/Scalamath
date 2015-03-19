package tests

import test.UnitTest
import meta._
import scala.collection.mutable.ListBuffer
import data._

object Meta extends UnitTest {
	val name = "Meta"

	def test {
		val list = (Link("element") | (Link("element") & Link("delimiter") & Link("this"))).p

		val testlist1 = list(Str("a", "a"), Str(",", "k"))
		val testlist2 = list(Str("a", "a"), Str(";", "k"))

		val all = Group("all")

		val variable = RE("[A-Za-z]+", "var")

		val operand = Group("operand")
		operand |+ Str("+", "plus")
		operand |+ Str("*", "times")
		operand |+ Str("-", "minus")
		val operation = all & operand & all

		val braces = Str("(", "bo") & all & Str(")", "bc")

		all |+ variable
		all |+ operation
		all |+ braces

		assert(variable.pars()("x") == List(Sign("x", "var")), "Simple Pars 1")
		assert(all.pars()("x+y").length == 1, "Simple Pars 2")
		assert(all.pars()("x+y+z").length == 2, "Simple Pars 3")
		assert(all.pars()("a+b+c+d").length == 5, "Simple Pars 4")

		assert(all.pars()("a+b+c+d").length == 5, "Complex Pars 1")

		assert(testlist1.pars()("a,a,a").length > 0, "Pattern 1")
		assert(testlist2.pars()("a;a;a").length > 0, "Pattern 2")
	}
}