package parser

import util.StringUtil._

//TODO Further Optimisation
case class ListP[+R](subs: List[Parser[R]]) extends Parser[List[R]] {
	def apply(str: String, depth: Int = 0) = if (depth < Parser.maxDepth) {
		if (subs.length > 1) {
			if (subs.exists(_.accelerated)) {
				val acc = subs.indexWhere(_.accelerated)
				val ind = subs(acc).possibleCuts(str)

				lazy val sub1 = ListP(subs.slice(0, acc))
				lazy val sub2 = ListP(subs.slice(acc + 1, subs.length))

				if (sub1.subs.length == 0) {
					val nInd = ind.filter(x => x._1 == 0)
					val p2 = nInd.map(x => str.slice(0, x._2)).map(subs(acc)(_))
					lazy val p3 = nInd.map(x => str.slice(x._2, str.length)).map(x => sub2(x))

					p2.zip(p3).flatMap {
						case (a, b) => b.flatMap { y => a.map { x => x :: y } }
					}
				} else if (sub2.subs.length == 0) {
					val nInd = ind.filter(x => x._1 + x._2 == str.length())

					val p2 = nInd.map(x => str.slice(x._1, x._1 + x._2)).map(subs(acc)(_))
					lazy val p1 = nInd.map(x => str.slice(0, x._1)).map(x => sub1(x))

					p2.zip(p1).flatMap {
						case (a, b) => b.flatMap { y => a.map { x => y ::: List(x) } }
					}
				} else {
					val p2 = ind.map(x => str.slice(x._1, x._1 + x._2)).map(subs(acc)(_))
					lazy val p1 = ind.map(x => str.slice(0, x._1)).map(x => sub1(x))
					lazy val p3 = ind.map(x => str.slice(x._1 + x._2, str.length)).map(x => sub2(x))

					p2.zip(p1).zip(p3).flatMap {
						case ((b, a), c) => b.flatMap { y => a.flatMap { x => c.map { z => x ::: List(y) ::: z } } }
					}
				}
			} else {
				((subs(0).minLength until Math.min(str.length - ListP(subs.tail).minLength, subs(0).maxLength) + 1)
					.map(x => (str.substring(0, x), str.substring(x))).flatMap {
						case (a, b) => {
							val f: Stream[R] = subs(0)(a, depth + 1)
							lazy val o: Stream[List[R]] = ListP(subs.splitAt(1)._2)(b, depth + 1)
							f.flatMap(x => o.map(y => x :: y))
						}
					}).toStream
			}
		} else if (subs.length == 1)
			subs(0)(str, depth + 1).map(List(_))
		else
			Stream(List())
	} else Stream()

	def minLength = subs.foldLeft(0)(_ + _.minLength)
	def maxLength =
		if (subs.find(_.maxLength == Int.MaxValue).isDefined)
			Int.MaxValue
		else
			subs.foldLeft(0)(_ + _.maxLength)

	val accelerated = subs.forall(_.accelerated)
	def possibleCuts(str: String) =
		if (subs.length == 1)
			subs(0).possibleCuts(str)
		else {
			val a = subs(0).possibleCuts(str)
			val b = ListP(subs.tail).possibleCuts(str)

			a.flatMap(x => b.map(y => (x, y)))
				.filter {
					case ((a, b), (c, d)) => a + b == c
				}.map {
					case ((a, b), (c, d)) => (a, b + d)
				}
		}
}