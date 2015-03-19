package parser

import util.StringUtil._

case class Simple(regex: String, min: Int = 0, max: Int = Int.MaxValue) extends Parser[String] {
	def apply(str: String, depth: Int = 0) = if (regex.r.findAllIn(str).contains(str)) Stream(str) else Stream()

	def minLength = min
	def maxLength = max

	val accelerated = true
	def possibleCuts(str: String) = regex.r.findAllMatchIn(str).map(x => (x.start, x.matched.length)).toStream
}