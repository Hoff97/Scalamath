package parser

import Stream._
import util.ListUtil._
import scala.reflect.runtime.universe._
import util._
import scala.collection.mutable.ListBuffer

trait Parser[+R] {
	def apply(str: String, depth: Int = 0): Stream[R]

	def minLength: Int
	def maxLength: Int

	val accelerated: Boolean
	def possibleCuts(str: String): Stream[(Int, Int)]
}

object Parser {
	var maxDepth = 20

	implicit class ListH1[R](x: ListP[R]) {
		def ++(y: Parser[R]) = ListP(x.subs ::: List(y))
	}

	implicit class ListH2[S, R <: S](x: Parser[R]) {
		def ++[T <: S](y: Parser[S]) = ListP(x :: y :: Nil)
	}

	implicit class OrHelper[R](x: Or[R]) {
		def |(y: Parser[R]) = Or[R]((y :: x.subs.toList).b)

		def p = replaceAll(x, (Var[R]("this"), x))
	}

	def replaceAll[S, T](x: Parser[S], replace: (Var[T], Parser[T]), already: List[Parser[_]] = List()): Parser[S] =
		if (already.contains(x))
			x
		else x match {
			case Var(n) => replace._2.asInstanceOf[Parser[S]]
			case TupleP(a, b) => TupleP(replaceAll(a, replace, already), replaceAll(b, replace, already))
			case TripleP(a, b, c) => TripleP(replaceAll(a, replace, already), replaceAll(b, replace, already), replaceAll(c, replace, already))
			case ListP(subs) => ListP(subs.map(replaceAll(_, replace, already)))
			case g @ Or(subs) => {
				subs.zipWithIndex.foreach {
					case (a, b) => subs.update(b, replaceAll(a, replace, g :: already))
				}
				g
			}
			case Trans(a, b) => Trans(replaceAll(a, replace, already), b)
			case _ => x
		}

	implicit class Helper[R](x: Parser[R]) {
		def &[T](oth: Parser[T]) = TupleP(x, oth)

		def |(obj: Parser[R]) = Or[R](List(x, obj).b)

		def ++(y: Parser[R]) =
			if (x.isInstanceOf[ListP[R]])
				if (y.isInstanceOf[ListP[R]])
					ListP(x.asInstanceOf[ListP[R]].subs ::: y.asInstanceOf[ListP[R]].subs)
				else
					ListP(x.asInstanceOf[ListP[R]].subs ::: List(y))
			else if (y.isInstanceOf[ListP[R]])
				ListP(x :: y.asInstanceOf[ListP[R]].subs)
			else
				ListP(List(x, y))
	}

	implicit class THelper5[R, S, T, U, V, W](x: TupleP[((((R, S), T), U), V), W]) {
		def #>[X](y: (R, S, T, U, V, W) => X) = Trans(x, (x: (((((R, S), T), U), V), W)) => y(x._1._1._1._1._1, x._1._1._1._1._2, x._1._1._1._2, x._1._1._2, x._1._2, x._2))
	}

	implicit class THelper4[R, S, T, U, V](x: TupleP[(((R, S), T), U), V]) {
		def #>[W](y: (R, S, T, U, V) => W) = Trans(x, (x: ((((R, S), T), U), V)) => y(x._1._1._1._1, x._1._1._1._2, x._1._1._2, x._1._2, x._2))
	}

	implicit class THelper7[R, S, T, U, V](x: TripleP[(R, S, T), U, V]) {
		def #>[W](y: (R, S, T, U, V) => W) = Trans(x, (x: ((R, S, T), U, V)) => y(x._1._1, x._1._2, x._1._3, x._2, x._3))
	}

	implicit class THelper3[R, S, T, U](x: TupleP[((R, S), T), U]) {
		def #>[V](y: ((R, S, T, U)) => V) = Trans(x, (x: (((R, S), T), U)) => y(x._1._1._1, x._1._1._2, x._1._2, x._2))
	}

	implicit class THelper2[R, S, T](x: TupleP[(R, S), T]) {
		def #>[U](y: ((R, S, T)) => U) = Trans(x, (x: ((R, S), T)) => y(x._1._1, x._1._2, x._2))
	}

	implicit class THelper33[R, S, T, U, V, W](x: TupleP[((R, S, T), U, V), W]) {
		def #>[X](y: ((R, S, T, U, V, W)) => X) = Trans(x, (x: (((R, S, T), U, V), W)) => y(x._1._1._1, x._1._1._2, x._1._1._3, x._1._2, x._1._3, x._2))
	}

	implicit class THelper32[R, S, T, U](x: TupleP[(R, S, T), U]) {
		def #>[V](y: ((R, S, T, U)) => V) = Trans(x, (x: ((R, S, T), U)) => y(x._1._1, x._1._2, x._1._3, x._2))
	}

	implicit class THelper1[R, S](x: TupleP[R, S]) {
		def #>[T](y: ((R, S)) => T) = Trans(x, y)
		def &[T](y: Parser[T]) = TripleP(x.s1, x.s2, y)
	}

	implicit class THelper[R](x: Parser[R]) {
		def #>[S](y: R => S) = Trans(x, y)
	}

	def recursive[_](x: Parser[_], already: List[Parser[_]] = List()): Boolean =
		if (already.contains(x)) true
		else x match {
			case x: Or[_] => x.subs.foldLeft(false) {
				case (a, b) => a || recursive(b, x :: already)
			}
			case ListP(subs) => subs.foldLeft(false) {
				case (a, b) => a || recursive(b, x :: already)
			}
			case Trans(sub, _) => recursive(sub, x :: already)
			case TupleP(a, b) => {
				val n = x :: already
				recursive(a, n) || recursive(b, n)
			}
			case TripleP(a, b, c) => {
				val n = x :: already
				recursive(a, n) || recursive(b, n) || recursive(c, n)
			}
			case _ => false
		}
}