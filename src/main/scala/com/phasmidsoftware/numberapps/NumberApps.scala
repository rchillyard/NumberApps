package com.phasmidsoftware.numberapps

import com.phasmidsoftware.number.core.{Constants, InfiniteSeries, L2, Number, Real, Transcendental}
import com.phasmidsoftware.number.core.Constants._
import com.phasmidsoftware.number.core.Constants.one
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.Number.NumberOps
import com.phasmidsoftware.number.expression.Exp
import com.phasmidsoftware.number.expression.Expression.ExpressionOps

import scala.util.Success

object NumberApps extends App {

  println("Hello, NumberApps!")

  /**
   * This should output the following: List(0, 1, 𝛑, 𝜀, 𝛗, √2, ∞)
   */
  val constants: Seq[String] = Seq(zero,one,pi,e,phi,root2, i, infinity) map (_.render)
  println(constants)

  /**
   * This demonstrates Euler's identity.
   * [[https://en.wikipedia.org/wiki/Euler%27s_identity]]
   * THe output should be -1.
    */
  val negOne = Constants.e power iPi
  println(negOne.render)

  /**
   * This demonstrates the "Basel" problem, another famous problem solved first by Euler.
   * [[https://en.wikipedia.org/wiki/Basel_problem]]
   * After showing the first ten terms of the series, it should output the following: 3.140637100985938±0.030%
   * NOTE that it converges slowly so it's probably not a good idea to reduce the tolerance much more.
   */
  val basel: InfiniteSeries[Number] = InfiniteSeries(LazyList.from(1).map(x => Rational(x).invert.square), 0.001)
  private val stringsBasel = basel.render(10)
  println(stringsBasel)
  private val xy = basel.evaluateToTolerance(0.000001)
  xy match {
    case Success(x) =>
      val pi = (6 * x).sqrt
      val piString = pi.render
      println(piString)
  }

  /**
   * The following should print i½𝛑
   */
  val x = i.ln
  println(x.render)

  /**
   * This should first print "ln(2)" followed by "2"
   */
  val y = L2
  println(y.render)
  val z = y.function(Exp)
  z.evaluate match {
    case Some(a) => println(a.render)
    case None =>
  }
}
