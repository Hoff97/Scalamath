package conversion

import data._
import util.ListUtil._

case class Translator(x: List[Rule]) {
	val rules = x.toList ::: Util.standardRules
	def translate(a: Individual): Option[Any] = rules.first(_.translate(a, translate))
}