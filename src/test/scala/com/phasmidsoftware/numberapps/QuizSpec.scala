package com.phasmidsoftware.numberapps

import com.phasmidsoftware.numberapps.Quiz.fib10
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.Await
import scala.util.Success

class QuizSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Quiz"
  it should "get the first 100 Fibonacci numbers" in {
    println(fib10.mkString(", "))
    Quiz.fib10.length shouldBe 10
    Quiz.fib10.last shouldBe 34
  }
  it should "get the Complex value" in {
    Quiz.c3.re shouldBe 1
    Quiz.c3.im shouldBe 1.5707963268 +- 1E-7
  }
  it should "get the factorial of 20" in {
    val eventualLong = Quiz.eventualFactorial20
    Await.ready(eventualLong, scala.concurrent.duration.Duration.Inf)
    eventualLong.value shouldBe Some(Success(2432902008176640000L))
  }
}
