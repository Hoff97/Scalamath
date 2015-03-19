package meta

import parser.Parser
import data.Individual
import util.ListUtil._

case class Pattern(c: Construct, names: List[String], name: String = "pattern") extends Construct {
	def pars(already: List[(Group, Parser[Individual])] = List()) = throw new Error("Cant generate Parser for Pattern " + name)

	def apply(c: Construct*): Construct = apply(c.toList)
	def apply(a: List[Construct]): Construct = replaceAll(c.copy, names.zip(a))

	def replaceAll(c: Construct, r: List[(String, Construct)], already: List[Construct] = Nil): Construct =
		if (already.contains(c))
			c
		else
			c match {
				case Link(n) => r.find(_._1 == n).map(_._2).getOrElse(c)
				case Group(s, n) =>
					if (already.length == 0) {
						val c = Group(List().b,n)
						s.foreach {
							case a => c |+ replaceAll(a, ("this", c) :: r, c :: already)
						}
						c
					} else {
						val c = Group(List().b,n)
						s.foreach {
							case a => c |+ replaceAll(a, r, c :: already)
						}
						c
					}
				case CList(s, n) => CList(s.map(x => replaceAll(x, r, already)), n)
				case _ => c
			}
}