package parser

case class Var[R](name: String) extends Parser[R] {
	def apply(str: String, depth: Int = 0) = throw new Error("Cant parse Link " + name)

	def minLength = throw new Error("Cant parse Link " + name)
	def maxLength = throw new Error("Cant parse Link " + name)
	
	val accelerated = false
	def possibleCuts(str: String) = throw new Error("Cant parse Link " + name)
}

object Var{
	implicit class Help[R](x: Var[R]){
		def &[S](y: Parser[S]) = TupleP(x,y)
	}
}