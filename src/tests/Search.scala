package tests

import test.UnitTest

import examples._
import util.Vector
import util.VectorUtil._
import search._

object Search extends UnitTest {
	val name = "Search"
	
	def test{
		TreeSearch.maxlevel = 100
		
		val a = new GridSearch(5.0~0.0,List())
		val b = new GridSearch(5.0~0.0,List(2.0~0.0))
		val c = new GridSearch(5.0~0.0,List(2.0~0.0,2.0~1.0,2.0~(-1.0),1.0~1.0,1.0~(-1.0),0.0~1.0,0.0~(-1.0),
				-1.0~1.0,-2.0~1.0,-2.0~(-1.0),-2.0~0.0))
		val d = new GridSearch(101.0~0.0,List())
		
		val s1 = a.search(0.0~0.0)
		val s2 = b.search(0.0~0.0)
		val s3 = c.search(0.0~0.0)
		val s4 = d.search(0.0~0.0)
		val s5 = d.search(1.0~0.0)
		
		assert(s1==List(5.0,4.0,3.0,2.0,1.0,0.0).map(_~0.0),"Simple Search 1")
		assert(s2(0)==5.0~0.0,"Simple Search 2")
		assert(s3(0)==5.0~0.0,"Simple Search 3")
		
		assert(s1.length<s2.length,"Search Lengths 1")
		assert(s2.length<s3.length,"Search Lengths 2")
		
		assert(s4==List(),"MaxLevel Search 1")
		assert(s5!=List(),"MaxLevel Search 2")
	}
}