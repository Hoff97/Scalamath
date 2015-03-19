package meta

import data._
import parser._
import scala.collection.mutable.ListBuffer
import util.ListUtil._

//TODO Optimize(Parsers run very slow)
//Maybe Implement Tuples as Alternative to List
trait Construct {
	def pars(already: List[(Group, Parser[Individual])] = List()): Parser[Individual]

	def name: String
}

object Construct {
	implicit class ConstructHelper(x: Construct) {
		def &(y: Construct) = CList(List(x, y), "list")
		def |(y: Construct) = Group(List(x, y).b, "group")
		def p = Pattern(x, links(x).filter(_ != "this").distinct)

		def copy = x match {
			case g @ Link(_) => g.copy()
			case g @ Group(_, _) => g.copy()
			case g @ CList(_, _) => g.copy()
			case g @ RE(_, _) => g.copy()
			case g @ Str(_, _) => g.copy()
			case g @ Pattern(_, _, _) => g.copy()
			case g @ IgnoreString(_) => g.copy()
		}
	}

	def links(c: Construct, already: List[Construct] = Nil): List[String] =
		if (already.contains(c))
			Nil
		else
			c match {
				case Link(n) => List(n)
				case Group(subs, _) => subs.foldLeft[List[String]](List()) {
					case (a, b) => a ::: links(b, c :: already)
				}
				case CList(subs, _) => subs.foldLeft[List[String]](List()) {
					case (a, b) => a ::: links(b, already)
				}
				case _ => Nil
			}

	def recursive(x: Construct, already: List[Construct] = Nil): Boolean =
		if (already.contains(x))
			true
		else
			x match {
				case g @ Group(_, _) =>
					if (g.subs.length == 0)
						true
					else
						g.subs.exists { a => recursive(a, x :: already) }
				case g @ CList(subs, _) => subs.exists { a => recursive(a, x :: already) }
				case Link(_) => true
				case _ => false
			}
}