/*
 * Copyright (c) 2026. Phasmid Software
 */
package com.phasmidsoftware.numberapps

import com.phasmidsoftware.number.core.inner.Rational

import scala.util.Success
import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.core.numerical.{InfiniteSeries, Number}
import com.phasmidsoftware.number.expression.expr.Root.{phi, rootTwo}
import com.phasmidsoftware.number.expression.expr.{ConstE, ConstI, ConstPi, E, Exp, Infinity, L2, One, Pi, Transcendental, Zero}

@main def exampleMainProgram(): Unit =
  import com.phasmidsoftware.number.top.expr.*

  // Method 1: Start with identity operator
  val expr0 = ∅ + 1 + 2 * 3 // lazy 7
  val expr1 = ∅ * 1 * 2 * 3 // lazy 6

  // Method 2: String interpolators
  val expr2 = math"1 + 2 * 3" // resulting type is eager 7
  val expr3 = lazymath"$expr1∧2 + 3 * $expr1 - 5" // resulting type is a simplified Expression with (lazy) value 49
  val expr4 = puremath"1 + 2 * 3" // resulting type is lazy 7

  // Method 3: Predefined constants
  val expr5 = one + 2 * 3 // lazy 7
  val expr6 = π / 2 // lazy ½𝛑
  val expr7 = sin(expr6) // lazy 1

  // Method 4: Explicit type annotation
  val expr8: Expression = 1 + 2 // lazy 3

  // All of these create Expression trees
  println(expr0.toLatex)
  println(expr1.toLatex)
  println(expr2.toLatex)
  println(expr3.toLatex)
  println(expr4.toLatex)
  println(expr5.toLatex)
  println(expr6.toLatex)
  println(expr7.toLatex)
  println(expr8.toLatex)

  println("Hello, NumberApps!")

  /**
   * This should output the following: List(0, 1, 𝛑, 𝜀, 𝛗, √2, ∞)
   */
  val constants: Seq[String] = {
    Seq(Zero, One, ConstPi, ConstE, phi, rootTwo, ConstI, Infinity) map (_.render)
  }
  println(constants)

  /**
   * This demonstrates Euler's identity.
   * [[https://en.wikipedia.org/wiki/Euler%27s_identity]]
   * THe output should be -1.
    */
  val negOne = ConstE ∧ ConstI
  println(negOne.render)

  /**
   * This demonstrates the "Basel" problem, another famous problem solved first by Euler.
   * [[https://en.wikipedia.org/wiki/Basel_problem]]
   * After showing the first ten terms of the series, it should output the following: 3.140637100985938±0.030%
   * NOTE that it converges slowly so it's probably not a good idea to reduce the tolerance much more.
   */
  val basel: InfiniteSeries[Number] = InfiniteSeries(LazyList.from(1).map(x => Rational(x).invert.square), 0.001)
  val stringsBasel = basel.render(10)
  println(stringsBasel)
  val xy = basel.evaluateToTolerance(0.000001)
  xy match {
    case Success(x) =>
      val pi = (6 * x).sqrt
      val piString = pi.render
      println(piString)
  }

  /**
   * The following should print i½𝛑
   */
  val x = ConstI.ln
  println(x.render)

  /**
   * This should first print "ln(2)" followed by "2"
   */
  val y = L2
  println(y.render)
  val z: Transcendental = y.function(Exp)
  z.evaluateAsIs match {
    case Some(a) => println(a.render)
    case None =>
  }

