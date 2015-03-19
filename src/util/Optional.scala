package util

abstract class Optional[+R <: P, +S <: P, +P] {
	def isA: Boolean
	def isB: Boolean
	def getA: R
	def getB: S
	def get: P
}
case class OA[+R <: P, +S <: P, P](value: R) extends Optional[R, S, P] {
	def isA = true
	def isB = false
	def getA = value
	def getB = throw new Exception("Not of Type")
	def get = value
}
case class OB[+R <: P, +S <: P, P](value: S) extends Optional[R, S, P] {
	def isA = false
	def isB = true
	def getA = throw new Exception("Not of Type")
	def getB = value
	def get = value
}