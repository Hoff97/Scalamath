package conversion

import data._
import meta.Language

object Util {
	val standardRules = List(new Rule {
		def translate(x: Individual, translate: Individual => Option[Any]) = x match {
			case Number(a) => Some(a)
			case Sign(a, _) => Some(a)
			case _ => None
		}
	})

	def fromFile(x: String) = {
		val f = scala.io.Source.fromFile(x).mkString.split("\r\n")
		val lang = Language.fromFile(f.head.split(" ")(1))

		val transl = Parser.translation(lang)

		val rules = transl match {
			case Some(x) => { f.tail.filter(_.length > 0).map(x(_)(0)).toList }
			case None => Nil
		}

		Translator(rules)
	}
}