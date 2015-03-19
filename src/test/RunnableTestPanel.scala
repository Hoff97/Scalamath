package test

import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.JTextPane
import javax.swing.text.StyledDocument
import javax.swing.JTextArea
import java.awt.Color
import javax.swing.tree.TreePath

abstract class RunnableTestPanel extends UnitTest {
	val s = new TestPanel
	
	var curNode = s.top
	
	def main(args: Array[String]) {
		val x = System.currentTimeMillis()
		test
		val p = Math.round(testList.count(x => x._1)/testList.length*100)
		curNode.setUserObject(curNode.getUserObject() + " - " + p + "% - " + (System.currentTimeMillis()-x) + "ms")
		s.tree.updateUI()
	}
	
	override def addTest(bool: Boolean,msg: String) {
		testList.push((bool,msg))
		curNode.add(new DefaultMutableTreeNode(msg))
		s.tree.updateUI()
	}
	
	override def assert(bool: Boolean,msg: String) {
		addTest(bool,msg)
		if(!bool)
			println(msg + " failed in " + name)
	}
	
	override def subTest(t: UnitTest){
		s.tree.expandPath(new TreePath(curNode))
		s.update(s.getGraphics())
		
		val x = System.currentTimeMillis()
		val nSub = new DefaultMutableTreeNode(t.name)
		curNode.add(nSub)
		s.tree.updateUI()
		val before = curNode
		curNode = nSub
		t.test
		t.testList.foreach(x => addTest(x._1, x._2))
		var p = 100
		if(t.testList.length>0)
			p = Math.round(t.testList.count(x => x._1)/t.testList.length*100)
		curNode.setUserObject(t.name + " - " + p + "% - " + (System.currentTimeMillis()-x) + "ms")
		s.tree.updateUI()
		curNode = before
	}
}