package conversion.instancers

import conversion.Instancer

case class StrConcat(i1: Instancer, i2: Instancer) extends Instancer {
	def instantiate(bind: List[(String, Any)]) =
		i1.instantiate(bind).flatMap(x => i2.instantiate(bind).map(y => x.toString() + y.toString()))
}