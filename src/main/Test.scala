package main

import conversion.instancers._
import util.ListUtil._
import meta.Language
import conversion.TranslationRule
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.Icon
import be.ugent.caagt.jmathtex.TeXFormula
import be.ugent.caagt.jmathtex.TeXIcon
import java.awt.Graphics

object Test {
	def main(args: Array[String]) {
		val a = (1 to 1000).map(x => BigInt(x).pow(x)).foldLeft(BigInt(0))((a, b) => a + b)
		println(a)
	}
}