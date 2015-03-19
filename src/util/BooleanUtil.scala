package util

object BooleanUtil {
	implicit class BoolUtil[T](x: Boolean) {
		def ?(a: T) = new Or(x,a)
		def ->(a: Boolean) = !(x&&(!a))
		def <->(a: Boolean) = (x&&a)||(!x&&(!a))
	}
	
	class Or[T](x: Boolean,a:T) {
		def |(b:T) = if(x) a else b
	}
}