package util

object Util {
	def subSets[T](t: List[T]): List[List[T]] = t match {
		case Nil => List(List())
		case head :: tail => {
			val rest = subSets(tail)
			rest.map(x => head :: x) ::: rest
		}
	}

	def isRegex(x: String) = try {
		val t = x.r
		true
	} catch {
		case _: Throwable => false
	}
}