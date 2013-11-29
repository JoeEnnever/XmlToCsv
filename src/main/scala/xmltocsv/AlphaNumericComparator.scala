package xmltocsv

import annotation.tailrec

/**
 *
 * <br>Date: 7/15/12
 * @author Joe Ennever
 */

object AlphaNumericComparator extends Ordering[String] {
  def compare(x: String, y: String) = {
    compareFrom(x, 0, y, 0)
  }

  @tailrec
  private def compareFrom(x: String, xi: Int, y: String, yi: Int): Int = {
    class IntLen(val v: Int, val vLen: Int)
    def digitsFrom(z: String, zi: Int): IntLen = {
      require(z.length > zi)
      var end = zi + 1
      while (end < z.length && z.charAt(end).isDigit) end += 1
      val dStr = z.substring(zi, end)
      new IntLen(dStr.toInt, end - zi)
    }
    if (xi >= x.length) {
      if (yi > y.length) return 0
      else return 1
    }
    if (yi >= y.length) {
      if (xi > x.length) return 0
      else return -1
    }
    val xc = x charAt xi
    val yc = y charAt yi
    if (xc.isDigit) {
      if (!yc.isDigit) 1
      else {
        val xDigits = digitsFrom(x, xi)
        val yDigits = digitsFrom(y, yi)
        val comp = xDigits.v compare yDigits.v
        if (comp != 0) comp
        else {
          compareFrom(x, xi + xDigits.vLen, y, yi + yDigits.vLen)
        }
      }
    }
    else {
      //!xc isDigit
      if (yc.isDigit) -1
      else {
        val charComp = xc compare yc
        if (charComp != 0) charComp
        else compareFrom(x, xi + 1, y, yi + 1)
      }
    }
  }
}
