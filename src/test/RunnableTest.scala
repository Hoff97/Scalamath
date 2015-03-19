package test

import scala.collection.mutable._

abstract class RunnableTest extends UnitTest {
	def main(args: Array[String]) {
		val tStart = System.currentTimeMillis();
		println("Running Test " + name + "\n===========================")
		test
		
		println("===========================")
		if(testList.count(_._1)<testList.length)
			println("Failure")
		print(testList.count(_._1))
		val p = Math.round(testList.count(_._1).toDouble/testList.length*100)
		println(" of " + testList.length + " succeeded (" + p + "%)")
		println("Needed " + (System.currentTimeMillis() - tStart) + "ms to complete all tests in " + name + ".")
	}
}