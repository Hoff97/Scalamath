package parser

import scala.collection.mutable._
import util.ListUtil._

case class Or[R](subs: ListBuffer[Parser[R]] = ListBuffer[Parser[R]]()) extends Parser[R] {
	def apply(str: String, depth: Int = 0) =
		if (depth < Parser.maxDepth) subs.toStream
			.flatMap(_(str, depth + 1))
		else Stream()

	def |+(x: Parser[R]) = { subs.+=(x); this }

	def minLength = {
		val f = subs.filter(!Parser.recursive(_))
		if (f.length == 0)
			0
		else
			f.minBy(_.minLength).minLength
	}
	def maxLength =
		if (subs.toList.exists(Parser.recursive(_)))
			Int.MaxValue
		else
			subs.toList.maxBy(_.maxLength).maxLength

	val accelerated =
		if (subs.length == 0)
			false
		else
			subs.forall(!Parser.recursive(_)) && subs.forall(_.accelerated)
	def possibleCuts(str: String) = subs.toStream.flatMap(_.possibleCuts(str))
}