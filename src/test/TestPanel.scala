package test

import javax.swing._
import java.awt.BorderLayout
import javax.swing.tree._
import java.awt.Color
import java.awt.ScrollPane

class TestPanel extends JFrame {
	setLayout(new BorderLayout)

	val top = new DefaultMutableTreeNode("All Tests");
    val tree = new JTree(top);
	
	val sc = new ScrollPane
	sc.add(tree)
	
	add(sc, BorderLayout.CENTER)
	
	pack();
	setVisible(true);
}