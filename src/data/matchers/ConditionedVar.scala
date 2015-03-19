package data.matchers

import math.Binding
import data._

abstract class ConditionedVar(name: String) extends Sign(name, "var") {
	override def matchWith(x: Individual) =
		if (tpe == "var") {
			if (condition(x))
				Some(List(new Binding(this, x)))
			else
				None
		} else Some(Nil)

	def condition(x: Individual): Boolean
}