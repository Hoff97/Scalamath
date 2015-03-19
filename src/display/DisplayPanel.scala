package display

import javax.swing.JPanel
import java.awt.Graphics
import be.ugent.caagt.jmathtex.TeXIcon

class DisplayPanel(x: TeXIcon) extends JPanel {
	override def paintComponent(g: Graphics) {
		super.paintComponent(g);
		x.paintIcon(this, g, 50, 50)
	}
}