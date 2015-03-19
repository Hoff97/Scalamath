package conversion

import parser.{ Or, Simple, PString }
import parser.AdditionalParsers._
import meta.Language
import conversion.instancers._

object Parser {
	val instancer: Or[Instancer] = Or()

	val variable = Simple("[A-Za-z]+") #> {
		case a => Var(a)
	}
	instancer |+ variable

	val num = Simple("""[0-9]+(\.[0-9]+)?""") #> {
		case a => Num(a.toDouble)
	}
	instancer |+ num

	val str = Simple("""\"[^\"]*\"""") #> {
		case a => Str(a.slice(1, a.length() - 1))
	}
	instancer |+ str

	val clsCreate = (Simple("""[A-Za-z\.]+""") & PString("(") & listWithEmpty(instancer, ";") & PString(")")) #> {
		case (name, _, arg, _) => ClassCreate(name, arg)
	}
	instancer |+ clsCreate

	val strConcat = (instancer & PString("+") & instancer) #> {
		case (a, _, b) => StrConcat(a, b)
	}
	instancer |+ strConcat

	def translation(x: Language) = x.get("all").map(x => (x._1.pars() & PString("=>") & instancer) #> {
		case (a, _, b) => TranslationRule(a, b)
	})
}