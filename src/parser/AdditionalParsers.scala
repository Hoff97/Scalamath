package parser

object AdditionalParsers {
	def list[R](element: Parser[R], delimiter: String = ",") = ((element #> { case a => List(a) }) |
		((element & PString(delimiter) & Var[List[R]]("this")) #> { case (a, _, b) => a :: b })).p

	def listWithEmpty[R](element: Parser[R], delimiter: String = ",") = (PString("") #> { case a => List[R]() }) | list(element, delimiter)

	def oneOrMore[R](element: Parser[R]) = list(element, "")

	def optional[R](element: Parser[R]) = (element #> { case x => Some(x).asInstanceOf[Option[R]] }) | (Simple("") #> { case _ => None })

	def zeroOrMore[R](element: Parser[R]) = PString("") #> { case _ => List[R]() } | oneOrMore(element)
}