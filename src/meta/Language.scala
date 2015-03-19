package meta

import util.ListUtil._
import Language._
import scala.collection.mutable._
import java.io.File
import meta.Parsers.definition
import meta.Parsers.pattern
import meta.Parsers.synSugar

class Language(_constructs: List[Constr] = List()) {
	val constructs: ListBuffer[Constr] = _constructs.b ++ Util.defaultStructs.b
	val pattern: ListBuffer[Pattern] = List().b

	def add(x: Constr) {
		val nx = (replaceLinks(applyPatterns(x.c, pattern.toList), constructs.map { x => x.c }.toList), x.parent)
		constructs += nx

		if (nx.parent.isDefined)
			constructs.find(_.name == nx.parent.get)
				.filter(_.c.isInstanceOf[Group])
				.map(_.c.asInstanceOf[Group])
				.foreach(_ |+ nx.c)
	}

	def addPattern(x: Pattern) {
		pattern += x
	}

	def get(x: String): Option[Constr] = constructs.find(x == _.name)

	def parse(x: String) = constructs.toList.flatMap(y => y.c.pars()(x)).distinct

	override def toString = constructs.map(x => constructStr(x.c)).mkString("\n")

	def properties {
		constructs.foreach { x => println(x.name + "--Accelerated:" + x.c.pars().accelerated) }
	}

	def constructStr(x: Construct, depth: Int = 0): String =
		constructs.find(_.c == x).filter(x => depth > 0).map(_.name)
			.getOrElse(x match {
				case CList(subs, _) => subs.map(constructStr(_, depth + 1)).mkString(",")
				case Group(subs, _) => subs.map(constructStr(_, depth + 1)).mkString("|")
				case RE(re, _) => "'" + re + "'"
				case Str(str, _) => "\"" + str + "\""
				case _ => x.toString
			})
}

object Language {
	type Constr = (Construct, Option[String])

	def replaceLinks(x: Construct, oth: List[Construct]): Construct =
		if (oth.contains(x))
			x
		else
			x match {
				case Link(name) => oth.find(name == _.name) match {
					case Some(a) => a
					case _ => x
				}
				case CList(subs, name) => CList(subs.map(replaceLinks(_, oth)), name)
				case Group(subs, name) => {
					subs.zipWithIndex.foreach {
						case (a, b) => subs.update(b, replaceLinks(a, x :: oth))
					}
					x
				}
				case SyntacticSugar(a, d, n) => SyntacticSugar(replaceLinks(a, oth), d, n)
				case _ => x
			}

	def replaceLinks(x: CList, oth: List[Construct]): CList = x match {
		case CList(subs, name) => CList(subs.map(replaceLinks(_, oth)), name)
	}

	def applyPatterns(x: Construct, patt: List[Pattern], already: List[Construct] = Nil): Construct =
		if (already.contains(x))
			x
		else
			x match {
				case PatApp(n, args, name) => Parsers.name(patt.find(_.name == n).map(x => x.apply(args)).getOrElse(x), name)
				case CList(subs, name) => CList(subs.map(applyPatterns(_, patt, already)), name)
				case Group(subs, name) => {
					subs.zipWithIndex.foreach {
						case (a, b) => subs.update(b, applyPatterns(a, patt, x :: already))
					}
					x
				}
				case _ => x
			}

	def recursive(x: Construct, already: List[Construct]): Boolean =
		if (already.contains(x)) true
		else
			x match {
				case CList(subs, name) => subs.exists(recursive(_, x :: already))
				case Group(subs, name) => subs.exists(recursive(_, x :: already))
				case _ => false
			}

	implicit class CHelper(x: Constr) {
		def c = x._1
		def name = x._1.name
		def parent = x._2
	}

	def fromFile(fileName: String): Language = {
		val ret = new Language()
		scala.io.Source.fromFile(fileName).mkString.split("\r\n").foreach(x => {
			if (x.length > 0) {
				if (x.startsWith("import")) {
					val path = x.split(" ")(1)
					println("Importing " + path)
					val i = Language.fromFile(path)
					i.constructs.foreach(x => ret.constructs += x)
				} else {
					val parsed = definition(x)
					if (parsed.length > 0)
						try {
							ret.add(parsed(0))
						} catch {
							case e: Throwable => e.printStackTrace() //println("Error when adding: " + x)
						}
					else {
						val c = pattern(x)
						if (c.length > 0) {
							ret.addPattern(c(0))
						} else {
							val d = synSugar(x)
							if (d.length > 0)
								ret.add(d(0))
							else
								println("Error when parsing: " + x)
						}
					}
				}
			}
		})
		ret
	}
}