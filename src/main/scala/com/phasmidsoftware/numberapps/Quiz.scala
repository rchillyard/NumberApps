package com.phasmidsoftware.numberapps


import com.phasmidsoftware.number.core.expression.ConstPi

import scala.concurrent.{Await, Future}

object Quiz {
  // NOTE in the following exercises, you MAY NOT use the method "get".

  val maybePi: Option[Double] = ConstPi.approximation.map(_.toDouble)
  val realPartAsAString = "1"
  val c1o: Option[Complex] = ??? // TODO the Complex value here should be (1,0) -- use Complex.apply with value from realPartAsAString
  val c2o: Option[Complex] = ??? // TODO the Complex value here should be (0,𝛑/2)
  val c3o: Option[Complex] = ??? // TODO the Complex value should be formed by adding the two Complex values above (from c1o and c2o)
  val c3: Complex = ??? // TODO get the complex value from c3o

  // TODO create a list of the first 10 Fibonacci numbers.
  // You don't have to do this any particular way, but you must use a LazyList.
  // The series begins 0, 1, 1, 2, 3, etc.
  val fib: LazyList[Long] = ??? // TODO get the (infinite) Fibonacci series
  val fib10: Seq[Long] = ??? // TODO take the first 10 elements of the Fibonacci series and convert it to a sequence with definite length.

  val eventualFactorial20: Future[Long] = ??? // TODO evaluate Future(20!) using the factorial method below.


  def factorial(n: Int): Long = ??? // TODO implement factorial in a tail-recursive way.
}

case class Complex(re: Double, im: Double):
  def +(that: Complex) = Complex(re + that.re, im + that.im)

object Complex:
  def apply(re: Double): Complex = ??? // TODO return a Complex value with re as the real part and 0 as the imaginary part
