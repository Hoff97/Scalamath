package data

import math.Binding

case class Sign(sign: String, tpe: String = "var") extends Individual {
	override def str = sign

	def matchWith(x: Individual) =
		if (tpe == "var") Some(List(new Binding(this, x)))
		else if (tpe == "ignore") Some(Nil)
		else {
			x match {
				case Sign(sign2, tpe2) => if (sign2 == sign && tpe2 == tpe) Some(Nil) else None
				case _ => None
			}
		}

	def bind(x: Binding) = x.fst match {
		case Sign(s, t) if s == sign && tpe == "var" => x.scnd
		case _ => this
	}

	def bindAll(x: List[Binding]) = x.find(x => x.fst match {
		case Sign(s, t) if s == sign && tpe == "var" => true
		case _ => false
	}).map(_.scnd).getOrElse(this)
}