package util

case class Vector(x: Double, y: Double) {
	def +(a: Vector) = Vector(x+a.x,y+a.y)
	def *(a: Double) = Vector(x*a,y*a)
	def *(a: Vector) = x*a.x+y*a.y
	def -(a: Vector) = this+(a*(-1))
	def /(a: Double) = this*(1/a)
	
	def length = Math.sqrt(x*x+y*y)
	
	def norm = this/length
	
	def angle(a: Vector) = Math.acos(this*a/(this.length*a.length))
}