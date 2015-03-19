package tests

import test.UnitTest
import parser._

object Parser extends UnitTest {
	val name = "Parser"
		
	def test{
		val operator = PString("+") | PString("*") | PString("^") | PString("/")
		
		val base = Or[Base]()
		val num = Simple("[0-9]+",1) #> { case a => Num(a.toInt) }
		val variable = Simple("[A-Za-z]+",1) #> { case a => Var(a) }
		val operation: Parser[Operation] = (base & operator & base) #> { case (a,b,c) => Operation(a,b,c) }
		val func : Parser[Fun] = (variable & PString("(") & base & PString(")")) #> {
			case (a,_,b,_) => Fun(a,b)
		}
		base |+ num
		base |+variable
		base |+ operation
		base |+ func
		
		val t = PString("a")
		val s = PString("b")
		val connectedTS = t & s
		
		val list1 = t++s
		val list2: ListP[Base] = num++operation
		val list3: ListP[Base] = operation++num++operation
		val list4: ListP[Base] = operation++operation++operation
		
		assert(operator("+")==List("+"),"Simple Plus Operator")
		assert(operator("^")==List("^"),"Simple Power Operator")
		assert(operator("*")==List("*"),"Simple Times Operator")
		assert(operator("-")==List(),"Not Operator")
		
		assert(num("143")==List(Num(143)),"Number Parsing 1")
		assert(num("25233254")==List(Num(25233254)),"Number Parsing 2")
		assert(num("345.23")==List(),"No Number Parsed")
		
		assert(variable("x")==List(Var("x")),"Simple Variable 1")
		assert(variable("Hallo")==List(Var("Hallo")),"Simple Variable 2")
		assert(variable("x2")==List(),"No Variable Parsed")
		
		assert(operation("x+y")==List(Operation(Var("x"),"+",Var("y"))),"Simple Operation 1")
		assert(operation("343*235")==List(Operation(Num(343),"*",Num(235))),"Simple Operation 2")
		assert(operation("343*235+x")==List(Operation(Operation(Num(343),"*",Num(235)),"+",Var("x")),
				Operation(Num(343),"*",Operation(Num(235),"+",Var("x")))),"Complex Operation 3")
		assert(operation("343*235+x+z").length==5,"Complex Operation 4")
		assert(operation("343*235+x+z+a").length==14,"Complex Operation 5")
				
		assert(operation("3x+5")==List(),"Operation Fail")
		
		assert(func("x(5)")==List(Fun(Var("x"),Num(5))),"Simple Function 1")
		assert(func("x(y(5))")==List(Fun(Var("x"),Fun(Var("y"),Num(5)))),"Simple Function 2")
		assert(func("x(2+y(5))")==List(Fun(Var("x"),Operation(Num(2),"+",Fun(Var("y"),Num(5))))),"Simple Function 3")
		
		assert(list1("ab")==List(List("a","b")),"Simple List 1")
		assert(list2("5a+b")==List(List(Num(5),Operation(Var("a"),"+",Var("b")))),"Simple List 2")
		assert(list3("x+y5a+b")==List(List(Operation(Var("x"),"+",Var("y")),Num(5),Operation(Var("a"),"+",Var("b")))),"Simple List 3")
		assert(list4("x+ya+bz*c")==List(List(Operation(Var("x"),"+",Var("y")),Operation(Var("a"),"+",Var("b")),Operation(Var("z"),"*",Var("c")))),"Simple List 4")
		
		assert(base("143")==List(Num(143)),"Or Parsing Number")
		assert(base("143.45")==List(),"Or Parsing Number Fail")
		assert(base("x")==List(Var("x")),"Or Parsing Variable")
		assert(base("x2")==List(),"Or Parsing Variable Fail")
		
		assert(base.minLength==1,"Or Minimum Length")
		assert(operation.minLength==3,"Tuple Minimum Length")
		assert(base.maxLength == Int.MaxValue,"Recursive Or Maximum Length")
		assert(operation.maxLength == Int.MaxValue,"Recursive Tuple Maximum Length")
		
		assert(operator.accelerated == true,"Accelerated Or")
		assert(variable.accelerated == true,"Accelerated RE 1")
		assert(num.accelerated == true,"Accelerated RE 2")
		assert(connectedTS.accelerated == true,"Accelerated Tuple")
		assert(list1.accelerated==true,"Accelerated List")
		
		assert(t.possibleCuts("aba") == List((0,1),(2,1)),"PString Cuts")
		assert(operator.possibleCuts("*+") == List((1,1),(0,1)),"Or Cuts")
		assert(connectedTS.possibleCuts("abaaba") == List((0,2),(3,2)),"Tuple Cuts")
		assert(list1.possibleCuts("abaaba") == List((0,2),(3,2)),"List Cuts")
	}
	
	trait Base
	case class Num(a: Int) extends Base {
		override def toString = a.toString
	}
	case class Var(a: String) extends Base{
		override def toString = a
	}
	case class Operation(a: Base, b: String, c: Base) extends Base {
		override def toString = "(" + a + b + c + ")"
	}
	case class Fun(a: Var, b: Base) extends Base {
		override def toString = a + "(" + b + ")"
	}
}