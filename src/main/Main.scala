package main

import examples.RuntimeLanguage._
import meta.Language
import data._
import examples.DFTest
import math.TestEval

import javax.swing.JFrame

object Main {
	def main(args: Array[String]) {
		val x = math.Util.fromFile("src/lang/derive.rule")
		inputEvaluator(x._1, math.Counters.function("d"), x._2)
	}
}