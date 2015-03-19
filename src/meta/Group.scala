package meta

import data._
import parser._
import scala.collection.mutable._
import util.ListUtil._

case class Group(subs: ListBuffer[Construct], name: String) extends Construct {
	def pars(already: List[(Group,Parser[Individual])] = List()) = if((already.find{ case(a,b) => a==this }).isEmpty){
		if(subs.exists { x => Construct.recursive(x) }){
			val pars = Or[Individual]()
		
			subs.foldLeft(pars) {
				case (a, b) => a |+ b.pars((this,pars) :: already)
			}
			
			pars
		} else {
			Or[Individual](subs.map(x => x.pars()))
		}
	} else (already.find{ case(a,b) => a==this }).get._2

	def |+(x: Construct) = { subs += (x); this }
}

object Group{
	def apply(x: String): Group = Group(new ListBuffer(),x)
}