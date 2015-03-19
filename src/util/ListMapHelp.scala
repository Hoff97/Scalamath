package util

import scala.language.dynamics
import scala.reflect._
import scala.reflect.runtime.universe._

class ListMapHelp[U: ClassTag](x: List[U]) extends Dynamic {
	def applyDynamic[V: ClassTag](name: String)(op: Any*): List[V] = x.map(y => {
		val mirror = runtimeMirror(y.getClass.getClassLoader).reflect(y)		
		val method = mirror.symbol.typeSignature.decls.find(name == _.name.decodedName.toString()).get
		
		mirror.reflectMethod(method.asMethod)(op(0)).asInstanceOf[V]
	})

	def selectDynamic[V: ClassTag](name: String): List[V] = x.map(y => {
		val mirror = runtimeMirror(y.getClass.getClassLoader).reflect(y)
		val field = mirror.symbol.typeSignature.members.find(name == _.name.decodedName.toString()).get

		if (field.isMethod)
			mirror.reflectMethod(field.asMethod)().asInstanceOf[V]
		else
			mirror.reflectField(field.asTerm).asInstanceOf[V]
	})
}