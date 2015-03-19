package util

import scala.annotation.tailrec

object FuncUtil {
	implicit class IFHelp[R](x: () => R) {
		def times(t: Int): List[R] = times(t, Nil)

		@tailrec
		final def times(t: Int, l: List[R]): List[R] =
			if (t > 0)
				times(t - 1, x() :: l)
			else
				l
	}
}