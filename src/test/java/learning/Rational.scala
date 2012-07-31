package learning

import annotation.tailrec

/**
 *
 * <br>Date: 6/26/12
 * @author Joe Ennever
 */

object Rational {
  implicit def int2RationalBuilder(numer: Int): RationalBuilder = new RationalBuilder(numer)

  implicit def int2Rational(x: Int) = new Rational(x)

  class RationalBuilder(numer: Int) {
    def \(denom: Int): Rational = new Rational(numer, denom)
  }

}

class Rational(numer: Int, denom: Int) {

  import Rational.int2RationalBuilder

  require(denom != 0, "cannot have 0 demoninator")

  def this(numer: Int) = this(numer, 1)


  private val g = gcd(numer.abs, denom.abs)
  val n = numer / g
  val d = denom / g

  override def toString = n + "\\" + d

  def +(that: Rational) = (n * that.d + that.n * d) \ (d * that.d)

  def *(that: Rational) = (n * that.n) \ (d * that.d)

  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}

