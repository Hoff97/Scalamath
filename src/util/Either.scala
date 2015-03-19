package util

trait EitherP[C,A<:C,B<:C] {
	def get: C
	def map[F,D<:F,E<:F](f1: A=>D,f2: B=>E): EitherP[F,D,E]
	
	val isA: Boolean
	val isB: Boolean
	
	def getA: A
	def getB: B
}

case class EA[C,A<:C,B<:C](x: A) extends EitherP[C,A,B] {
	def get = x
	
	def map[F,D<:F,E<:F](f1: A=>D,f2: B=>E) = EA[F,D,E](f1(x))
	
	val isA = true
	val isB = false
	
	def getA = x
	def getB = throw new Exception("Cant get B, is of type A")
}

case class EB[C,A<:C,B<:C](x: B) extends EitherP[C,A,B] {
	def get = x
	
	def map[F,D<:F,E<:F](f1: A=>D,f2: B=>E) = EB[F,D,E](f2(x))
	
	val isA = false
	val isB = true
	
	def getA = throw new Exception("Cant get A, is of type B")
	def getB = x
}