package data

import math.Binding

case class Struct(subs: List[Individual], tpe: String) extends Individual {
	override def str = subs.map(_.str).mkString("")

	def matchWith(x: Individual) =
		if (subs.filterNot(isIgnoreable(_)).length == 1)
			subs.find(!isIgnoreable(_)).map(_.matchWith(x)) match {
				case Some(x) => x
				case _ => None
			}
		else
			x match {
				case Struct(_subs, _tpe) =>
					if (_tpe == tpe && subs.length == _subs.length) {
						val sm = subs.zip(_subs).map { case (a, b) => a.matchWith(b) }
						if (sm.contains(None))
							None
						else if (Util.consistent(sm.flatMap(_.get))) //Check!!
							Some(sm.flatMap(_.get))
						else
							None
					} else
						None
				case _ => None
			}

	def bind(x: Binding) = Struct(subs.map(_.bind(x)), tpe)
	def bindAll(x: List[Binding]) = Struct(subs.map(_.bindAll(x)), tpe)

	def isIgnoreable(x: Individual) = x match {
		case Sign(a, "ignore") => true
		case _ => false
	}
}