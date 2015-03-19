package util

object VectorUtil {
	implicit class DoubleToVector(x: Double){
		def ~(y: Double) = Vector(x,y)
	}
}