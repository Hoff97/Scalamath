package parser

import util.ListUtil._

abstract class TupleHelper[T](subs: List[Parser[_]]) extends Parser[T] {
	def minLength = subs.foldLeft(0)(_ + _.minLength)
	def maxLength =
		if (subs.exists(_.maxLength == Int.MaxValue))
			Int.MaxValue
		else
			subs.foldLeft(0)(_ + _.maxLength)
}
//TODO: Further Optimization
case class TupleP[R, S](s1: Parser[R], s2: Parser[S]) extends TupleHelper[(R, S)](List(s1, s2)) {
	def apply(str: String, depth: Int = 0) = if (depth < Parser.maxDepth) {
		if (s1.accelerated) {
			val cuts = s1.possibleCuts(str).filter(x => x._1 == 0)
			val interp1 = cuts.map(x => str.slice(x._1, x._2)).map(x => s1(x, depth + 1))
			lazy val interp2 = cuts.map(x => str.slice(x._2, str.length)).map(x => s2(x, depth + 1))
			interp1.zip(interp2).flatMap {
				case (a, b) => a.zip(b)
			}
		} else if (s2.accelerated) {
			val cuts = s2.possibleCuts(str).filter(x => x._1 + x._2 == str.length)
			val interp1 = cuts.map(x => str.slice(0, x._1)).map(x => s1(x, depth + 1))
			lazy val interp2 = cuts.map(x => str.slice(x._1, str.length)).map(x => s2(x, depth + 1))
			interp1.zip(interp2).flatMap {
				case (a, b) => a.zip(b)
			}
		} else {
			(Math.max(s1.minLength, str.length - s2.maxLength) until Math.min(str.length - s2.minLength, s1.maxLength) + 1)
				.map(x => (str.substring(0, x), str.substring(x))).toStream.flatMap {
					case (a, b) => {
						val f: Stream[R] = s1(a, depth + 1)
						lazy val o = s2(b, depth + 1)
						f.flatMap(x => o.map(y => (x, y)))
					}
				}
		}
	} else Stream()

	val accelerated = s1.accelerated && s2.accelerated
	def possibleCuts(str: String) = {
		val cuts1 = s1.possibleCuts(str)
		lazy val cuts2 = s2.possibleCuts(str)

		cuts1.flatMap(x => cuts2.map(y => (x, y)))
			.filter {
				case ((a, b), (c, d)) => a + b == c
			}.map {
				case ((a, b), (c, d)) => (a, b + d)
			}
	}
}

case class TripleP[R, S, T](s1: Parser[R], s2: Parser[S], s3: Parser[T]) extends TupleHelper[(R, S, T)](List(s1, s2, s3)) {
	def apply(str: String, depth: Int = 0) = if (depth < Parser.maxDepth) {
		if (s1.accelerated) {
			val cuts = s1.possibleCuts(str).filter(x => x._1 == 0)
			val interp1 = cuts.map(x => str.slice(0, x._2)).map(x => s1(x, depth + 1))
			lazy val newTuple = TupleP(s2, s3)
			lazy val interp2 = cuts.map(x => str.slice(x._2, str.length)).map(x => newTuple(x, depth + 1))
			interp1.zip(interp2).flatMap {
				case (a, b) => a.flatMap { x => b.map(y => (x, y._1, y._2)) }
			}
		} else if (s2.accelerated) {
			val cuts = s2.possibleCuts(str)
			val interp1 = cuts.map(x => str.slice(0, x._1)).map(x => s1(x, depth + 1))
			lazy val interp3 = cuts.map(x => str.slice(x._1 + x._2, str.length)).map { x => s3(x, depth + 1) }
			lazy val interp2 = cuts.map(x => str.slice(x._1, x._1 + x._2)).map { x => s2(x, depth + 1) }
			interp1.zip(interp2).zip(interp3).flatMap {
				case ((a, b), c) => a.flatMap(x => b.flatMap(y => c.map(z => (x, y, z))))
			}
		} else if (s3.accelerated) {
			val cuts = s3.possibleCuts(str).filter(x => x._1 + x._2 == str.length())
			val interp1 = cuts.map(x => str.slice(x._1, str.length)).map(x => s3(x, depth + 1))
			lazy val newTuple = TupleP(s1, s2)
			lazy val interp2 = cuts.map(x => str.slice(0, x._1)).map(x => newTuple(x, depth + 1))
			interp1.zip(interp2).flatMap {
				case (a, b) => a.flatMap { x => b.map(y => (y._1, y._2, x)) }
			}
		} else {
			(Math.max(s1.minLength, str.length - s2.maxLength) until Math.min(str.length - s2.minLength, s1.maxLength) + 1)
				.map(x => (str.substring(0, x), str.substring(x))).toStream.flatMap {
					case (a, b) => {
						val f: Stream[R] = s1(a, depth + 1)
						lazy val o = TupleP(s2, s3)(b, depth + 1)
						f.flatMap(x => o.map(y => (x, y._1, y._2)))
					}
				}
		}
	} else Stream()

	val accelerated = s1.accelerated && s2.accelerated && s3.accelerated
	def possibleCuts(str: String) = {
		val cuts1 = s1.possibleCuts(str)
		lazy val cuts2 = s2.possibleCuts(str)
		lazy val cuts3 = s3.possibleCuts(str)

		cuts1.flatMap(x => cuts2.flatMap(y => cuts3.map(z => (x, y, z))))
			.filter {
				case ((a, b), (c, d), (e, f)) => a + b == c && c + d == e
			}.map {
				case ((a, b), (c, d), (e, f)) => (a, b + d + f)
			}
	}
}