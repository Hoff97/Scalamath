package display

import data.Individual
import be.ugent.caagt.jmathtex.TeXFormula
import be.ugent.caagt.jmathtex.TeXIcon
import javax.swing.JFrame

object Util {
	val translator = conversion.Util.fromFile("src/lang/latex.trans")

	def displayFormula(x: Individual) {
		val a = translator.translate(x) match {
			case Some(a: String) => a
			case _ => "Übersetzung fehlgeschlagen"
		}

		println(a)

		val formula = new TeXFormula(a)
		val icon = formula.createTeXIcon(0, 50.0f)

		val pane = new DisplayPanel(icon)
		val frame = new JFrame("Formula-Display")

		frame.add(pane)

		frame.pack()
		frame.setVisible(true)

		frame.setSize(500, 200)
	}
}