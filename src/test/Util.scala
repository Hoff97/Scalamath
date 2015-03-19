package test

import util.FuncUtil._

object Util {
	def avgRuntime[R](x: () => R, sRes: Boolean = true, t: Int = 100, name: String = "t") {
		val time = System.currentTimeMillis().toDouble
		val res = x.times(t)
		val tN = System.currentTimeMillis().toDouble
		val avg = (tN - time) / t
		println("Average time to do action '" + name + "' is " + avg + " ms.")
		if (sRes) {
			println("Result:")
			println(res(0))
		}
	}
}