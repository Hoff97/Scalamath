package conversion.instancers

import conversion.Instancer

case class Str(str: String) extends Instancer {
	def instantiate(bind: List[(String, Any)]) = Some(str)
}