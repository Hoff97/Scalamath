package math

import data._
import scala.collection.mutable._
import util.ListUtil._
import search._

class SimpleEvaluator(x: TreeSearch[Individual, TreeNode[Individual]]) extends Evalutator {
	def eval(a: Individual) = x.search(a).head
}