package util

import scala.collection.mutable._
import scala.reflect._

object ListUtil {
	implicit class ListUtils[T: ClassTag](t: List[T]) {
		def foreachWhile[B](func: T => B, con: (T, B) => Boolean, list: List[T] = t): Unit = list match { //Executes Action func on each Element while con is true
			case head :: tail => {
				val value = func(head)
				if (con(head, value)) foreachWhile(func, con, tail)
			}
			case _ =>
		}

		def mapWhile[B](f: T => B)(cond: (T, B) => Boolean): List[B] = { //Maps List to B by f while cond is true
			var newList = List[B]()
			foreachWhile(x => {
				val n = f(x)
				newList = newList :+ n
				n
			}, cond)
			newList
		}

		def flatMapWhile[B](f: T => List[B])(cond: (T, List[B]) => Boolean): List[B] =
			t.mapWhile(f)(cond).flatMap(x => x)

		def subLists(length: Int): List[List[T]] = (for (i <- 0 until t.length - length + 1) yield t.slice(i, i + length)).toList

		def *(n: Int) = (for (_ <- 0 until n) yield t).toList

		def mh = new ListMapHelp(t)

		def b = {
			val x = new ListBuffer[T]()
			t.foreach(x += _)
			x
		}

		def pars: List[(T, T)] = t match {
			case head :: tail => tail.map((head, _)) ::: tail.pars
			case _ => List()
		}

		def tryMap[B](f: T => B): List[B] = t match {
			case head :: tail => try {
				f(head) :: tail.tryMap(f)
			} catch {
				case _: Throwable => tail.tryMap(f)
			}
			case _ => List()
		}

		def first[B](f: T => Option[B]): Option[B] = t match {
			case h :: t => f(h) match {
				case g @ Some(_) => g
				case _ => t.first(f)
			}
			case _ => None
		}
	}

	implicit class OptUtil[T](x: List[Option[T]]) {
		def optionOut: Option[List[T]] = x match {
			case Some(x) :: y => y.optionOut.map(x :: _)
			case None :: y => None
			case _ => Some(Nil)
		}
	}
}