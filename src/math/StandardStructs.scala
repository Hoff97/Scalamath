package math

import data._

object StandardStructs {
	def bool(x: Boolean) =
		if (x)
			Sign("W", "Sign")
		else
			Sign("F", "Sign")
}