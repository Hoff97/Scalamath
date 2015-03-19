package conversion.instancers

import conversion.Instancer

case class Var(name: String) extends Instancer {
	def instantiate(bind: List[(String, Any)]) = bind.find(x => x._1 == name).map(_._2)
}