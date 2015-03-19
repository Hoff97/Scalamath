package util

object StringUtil {
	implicit class StrHelper(a: String){
		def findIndizesIn(x: String) = 
		for(i <- 0 to x.length-a.length
			if x.slice(i, x.length).startsWith(a))
			yield i
	}
}