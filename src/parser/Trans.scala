package parser

case class Trans[R, S](pars: Parser[R], trans: R => S) extends Parser[S] {
	def apply(str: String,depth: Int = 0) = pars(str,depth).map(x => trans(x))
	
	def minLength = pars.minLength
	def maxLength = pars.maxLength
	
	val accelerated = pars.accelerated
	def possibleCuts(str: String) = pars.possibleCuts(str)
}