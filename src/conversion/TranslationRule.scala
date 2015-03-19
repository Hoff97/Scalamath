package conversion

import data._
import math._
import util.ListUtil._

trait Rule {
	def translate(x: Individual, translate: Individual => Option[Any]): Option[Any]
}

case class TranslationRule(pattern: Individual, inst: Instancer) extends Rule {
	def translate(x: Individual, translate: Individual => Option[Any]) =
		pattern.matchWith(x)
			.flatMap(x => x.map(y => TranslationRule.bindToTuple(y, translate)).optionOut)
			.flatMap(x => inst.instantiate(x))
}

object TranslationRule {
	def bindToTuple(x: Binding, translate: Individual => Option[Any]) = x.fst match {
		case Sign(a, _) => translate(x.scnd).map((a, _))
		case _ => None
	}
}