package test

import scala.collection.mutable._

abstract class UnitTest {
	val testList: Stack[(Boolean,String)] = new Stack
	
	def addTest(bool: Boolean,msg: String) {
		testList.push((bool,msg))
	}
	
	def assert(bool: Boolean,msg: String) {
		addTest(bool,msg)
		if(!bool)
			println(msg + " failed in " + name)
	}
	
	def subTest(t: UnitTest){
		val x = System.currentTimeMillis()
		t.test
		t.testList.foreach(testList.push(_))
		println("Completed " + t.name + " in " + (System.currentTimeMillis()-x) + "ms.")
	}
	
	def test
	val name: String
}