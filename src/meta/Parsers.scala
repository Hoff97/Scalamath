package meta

import parser._
import util.Util._
import util.ListUtil._
import scala.collection.mutable._
import Language._
import parser.AdditionalParsers._
import data._

object Parsers {
	val construct = Or[Construct]()
	//TODO Implement Patterns as Parsers and into language
	val re: Parser[RE] = Simple("'.*'", 3) #> { case a if isRegex(a.substring(1, a.length() - 1)) => RE(a.substring(1, a.length() - 1), "re") }

	val str: Parser[Str] = Simple("\"[^\"]*\"", 2, 50) #> { case a => Str(a.substring(1, a.length() - 1), "str") }

	val iStr: Parser[IgnoreString] = Simple("°[^°]*°", 2, 50) #> { case a => IgnoreString(a) }

	val list: Parser[CList] = (construct & PString(",") & construct) #> {
		case (a, _, b) => a match {
			case CList(subs1, name1) => b match {
				case CList(subs2, name2) => CList(subs1 ::: subs2, "list")
				case _ => CList(subs1 ::: List(b), "list")
			}
			case _ => b match {
				case CList(subs2, name2) => CList(a :: subs2, "list")
				case _ => CList(List(a, b), "list")
			}
		}
	}

	val braces: Parser[Construct] = ((PString("(") & construct & PString(")")) #> {
		case (_, a, _) => a
	})

	val link: Parser[Link] = Simple("[A-Za-z]+", 1, 30) #> { case a => Link(a) }

	val applicationList = AdditionalParsers.list(construct, ";")
	val application = (link & PString("[") & applicationList & PString("]")) #> { case (a, _, b, _) => PatApp(a.name, b) }

	val group1: Parser[Group] = (construct & PString("|") & construct) #> {
		case (a, _, b) => a match {
			case Group(l, _) => b match {
				case Group(d, _) => Group((l.toList ::: (d.toList)).b, "g")
				case a => Group((l.toList ::: List(a)).b, "g")
			}
			case a => b match {
				case Group(d, _) => Group((a :: (d.toList)).b, "g")
				case b => Group(List(a, b).b, "g")
			}
		}
	}

	val group2: Parser[Group] = (PString("|") & PString("|")) #> {
		case (_, _) => Group(ListBuffer(), "group")
	}

	val group = Or[Group](List(group1, group2).b)

	construct |+ re
	construct |+ list
	construct |+ str
	construct |+ link
	construct |+ group
	construct |+ braces
	construct |+ application
	construct |+ iStr

	val name: Parser[String] = (PString("DEFINE ") & Simple("[A-Za-z]+", 1, 30)) #> { case (_, a) => a }
	val parent1: Parser[Option[String]] = (PString(" : ") & Simple("[A-Za-z]+", 1, 30)) #> { case (_, a) => Some(a) }
	val parent2: Parser[Option[String]] = PString("") #> { case _ => None }
	val parent: Parser[Option[String]] = Or(List(parent1, parent2).b)
	val block: Parser[Construct] = (PString(" = ") & construct) #> { case (_, a) => a }
	val definition: Parser[Constr] = (name & parent & block) #> { case (a, b, c) => (name(c, a), b) }

	val patternname = (link & PString("[") & AdditionalParsers.list(link, ";") & PString("]")) #> { case (a, _, b, _) => (a.name, b.map(_.name)) }
	val pattern = (PString("DEFINE ") & patternname & PString(" = ") & construct) #> { case (_, p, _, c) => Pattern(c, p._2, p._1) }

	val dat = Or[Individual]()
	dat |+ (Simple("[0-9]+")) #> { case a => Sign(a, "link") }
	val struct = (Simple("[A-Za-z]+") & PString("(") & AdditionalParsers.list(dat) & PString(")")) #> {
		case (a, _, b, _) => Struct(b, a)
	}
	dat |+ struct
	dat |+ ((PString("\"") & Simple("[A-Za-z0-9]+") & PString("\"")) #> { case (_, b, _) => Sign(b, "sign") })
	dat |+ ((PString("'") & Simple("[A-Za-z0-9]+") & PString("'")) #> { case (_, b, _) => Sign(b, "var") })

	val sf = (PString("DEFINE ") & Simple("[A-Za-z]+") & PString(" ") & list & parent) #> { case (_, a, _, b, c) => (a, b, c) }

	val synSugar = (sf & PString(" = ") & dat) #> { case (a, b, c, _, d) => (SyntacticSugar(b, d, a), c) }

	def name(x: Construct, name: String): Construct = x match {
		case CList(subs, _) => CList(subs, name)
		case Group(subs, _) => Group(subs, name)
		case RE(re, _) => RE(re, name)
		case Str(str, _) => Str(str, name)
		case PatApp(n, args, _) => PatApp(n, args, name)
		case _ => x
	}
}