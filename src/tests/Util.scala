package tests

import test.UnitTest

import util.ListUtil._

object Util extends UnitTest {
	val name = "Util"
		
	def test {
		val t = List(1,2,3,4)
		
		val a = t.mapWhile(x => x*x){ case (a,b) => a<3 }
		val b = t.mapWhile(x => x*x){ case (a,b) => b<9 }
		assert(a==List(1,4,9),"MapWhile1")
		assert(b==List(1,4,9),"MapWhile1")
		
		val c = t.flatMapWhile(x => (1 to x).toList){ case(a,b) => a<3 }
		val d = t.flatMapWhile(x => (1 to x).toList){ case(a,b) => b.length<3 }
		assert(c==List(1,1,2,1,2,3),"FlatMap1")
		assert(d==List(1,1,2,1,2,3),"FlatMap2")
		
		assert(t.subLists(2)==List(List(1,2),List(2,3),List(3,4)),"SubLists")
		assert(t*3==List(t,t,t),"Times")
	}
}