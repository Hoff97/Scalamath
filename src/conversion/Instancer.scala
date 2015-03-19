package conversion

trait Instancer {
	def instantiate(bind: List[(String, Any)]): Option[Any]
}