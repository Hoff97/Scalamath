package parser

import util.StringUtil._

case class PString(str: String) extends Parser[String] {
	def apply(str2: String, depth: Int = 0) = if (str == str2) Stream(str) else Stream()

	def minLength = str.length()
	def maxLength = str.length()

	val accelerated = true
	def possibleCuts(s: String) = str.findIndizesIn(s).toList.map(x => (x, str.length)).toStream
}