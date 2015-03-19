package data

import math.Binding

case class Number(x: Double) extends Individual {
	def tpe = "num"

	def str = x.toString

	def matchWith(y: Individual) = y match {
		case Number(z) if z == x => Some(List())
		case _ => None
	}

	def bind(x: Binding) = this
	def bindAll(x: List[Binding]) = this
}