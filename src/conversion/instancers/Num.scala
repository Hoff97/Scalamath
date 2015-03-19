package conversion.instancers

import conversion.Instancer

case class Num(a: Double) extends Instancer {
	def instantiate(bind: List[(String, Any)]) = Some(a)
}