package tests

import test.RunnableTest

object All extends RunnableTest {
	val name = "All"
	
	def test {
		subTest(Parser)
		subTest(Search)
		subTest(Meta)
		subTest(Data)
		subTest(Math)
		subTest(Util)
	}
}