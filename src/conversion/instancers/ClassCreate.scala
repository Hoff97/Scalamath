package conversion.instancers

import conversion.Instancer
import util.ListUtil._
import java.lang.reflect.Constructor

case class ClassCreate(name: String, subs: List[Instancer]) extends Instancer {
	def instantiate(bind: List[(String, Any)]) =
		if (ClassCreate.specials.contains(name))
			subs.map(_.instantiate(bind)).optionOut.flatMap(x => ClassCreate.specialInst(name, x))
		else
			subs.map(_.instantiate(bind)).optionOut.flatMap(x => ClassCreate.create(x, Class.forName(name)))
}

object ClassCreate {
	val specials = List("List")

	def specialInst(name: String, instances: List[Any]) = name match {
		case "List" => Some(instances)
		case _ => None
	}

	def create(x: List[Any], cls: Class[_]) = x match {
		case a :: Nil => cls.getConstructors.toList.first(applyOpt(x => x.newInstance(a.asInstanceOf[Object])))
		case a :: b :: Nil => cls.getConstructors.toList.first(applyOpt(x => x.newInstance(a.asInstanceOf[Object], b.asInstanceOf[Object])))
		case a :: b :: c :: Nil => cls.getConstructors.toList.first(applyOpt(x => x.newInstance(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object])))
		case a :: b :: c :: d :: Nil => cls.getConstructors.toList.first(applyOpt(x => x.newInstance(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object], d.asInstanceOf[Object])))
		case a :: b :: c :: d :: e :: Nil => cls.getConstructors.toList.first(applyOpt(x => x.newInstance(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object], d.asInstanceOf[Object], e.asInstanceOf[Object])))
		case _ => None
	}

	def applyOpt(a: Constructor[_] => Any)(x: Constructor[_]) =
		try {
			Some(a(x))
		} catch {
			case e: Exception => None
		}
}