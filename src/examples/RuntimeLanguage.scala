package examples

import meta.Parsers._
import meta._
import scala.io._
import data._
import math._

object RuntimeLanguage {
	def inputParser(x: Language) {
		val test = x

		var input = ""
		var data: List[Individual] = List()

		println("Start")

		while (input != "0") {
			input = StdIn.readLine
			val p = test.parse(input)
			data = test.parse(input) ::: data
			println("Added Data:")
			p.foreach(x => {
				val d = math.Util.simplify(x)
				println(d.map(_.str))
			})
		}
	}

	def inputEvaluator(x: Language, a: Individual => Boolean, r: List[Rule] = Nil) {
		parser.Parser.maxDepth = 20
		var data: List[Individual] = Nil
		var rules: List[Rule] = r
		var search = new CountSearch(a, rules)

		println("Ready for input:")

		var input = ""

		while (input != "end") {
			input = StdIn.readLine
			val t = System.currentTimeMillis()
			val p = x.parse(input)
			println("Parsed. " + (System.currentTimeMillis() - t) + "ms")

			p match {
				case (g @ Struct(List(s1, _, s2), "rule")) :: _ => {
					rules = StructedRule(s1, s2) :: rules
					search = new CountSearch(a, rules)
					println("Added rule: " + g.str)
				}
				case x :: _ => {
					val a = search.search(x)
					data = a.head :: data
					println("Evaluated and added data: " + a.head.str)
					display.Util.displayFormula(a.head)
					println("Evalutation chain(" + a.length + " steps): " + a.map(_.str))
				}
				case _ => println("Couldnt Parse")
			}
		}
	}
}