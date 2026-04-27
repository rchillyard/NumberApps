package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.{E, Infinity, MinusOne, Zero, One as ExprOne}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EmlSpec extends AnyFlatSpec with Matchers {

  behavior of "Eml"

  it should "handle one" in {
    val one: S = Eml.one
    one.render shouldBe "1"
    one.asExpression shouldBe ExprOne
  }
  it should "handle E" in {
    val e: S = Eml.e
    e.render shouldBe "eml(1,1)"
    e.asExpression shouldBe E
    e.ln.render shouldBe "eml(1,eml(eml(1,eml(1,1)),1))"
    e.ln.asExpression shouldBe ExprOne
  }
  it should "handle zero" in {
    val zero = Eml.zero
    zero.render shouldBe "eml(1,eml(eml(1,1),1))"
    zero.asExpression shouldBe Zero
    zero.exp.asExpression shouldBe ExprOne
  }
  it should "handle Eml(Eml.e,1)" in {
    val ee: S = Eml(Eml.e, 1)
    ee.render shouldBe "eml(eml(1,1),1)"
    ee.asExpression shouldBe E.exp
    ee.ln.render shouldBe "eml(1,eml(eml(1,eml(eml(1,1),1)),1))"
    ee.ln.asExpression shouldBe E
  }
  it should "handle Eml(1, Eml.e)" in {
    val e: S = Eml(1, Eml.e)
    e.render shouldBe "eml(1,eml(1,1))"
    e.asExpression shouldBe (MinusOne + E)
  }
  it should "handle Eml(Eml.zero,1)" in {
    val e: S = Eml(Eml.zero, 1)
    e.render shouldBe "eml(eml(1,eml(eml(1,1),1)),1)"
    e.asExpression shouldBe ExprOne
    e.ln.render shouldBe "eml(1,eml(eml(1,eml(eml(1,eml(eml(1,1),1)),1)),1))"
    e.ln.asExpression shouldBe Zero
  }
  it should "handle Eml(1, Eml.zero)" in {
    val e: S = Eml(1, Eml.zero)
    e.render shouldBe "eml(1,eml(1,eml(eml(1,1),1)))"
    e.asExpression shouldBe (E + Infinity)
  }
  it should "expand One" in {
    val expand: Seq[S] = One.expand
    expand shouldBe Seq(Eml.e)
  }
  it should "expand Eml.e" in {
    val expand: Seq[S] = Eml.e.expand
    expand shouldBe List(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))
  }
  it should "expand Eml.e.expand" in {
    val expand: Seq[S] = Eml.e.expand
    val expansion = for (e <- expand; x <- e.expand) yield x
    println(expansion.mkString(", "))
    expansion shouldBe List(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))
  }
  it should "find all expressions" in {
    val s0 = One
    val s1: Eml = Eml.e.asInstanceOf[Eml]
    val s20 = Eml(s0, s1)
    val s21 = Eml(s1, s0)
    val s22 = Eml(s1, s1)
    val s201 = Eml(s20, s1)
    val s202 = Eml(s1, s20)
    val s203 = Eml(s20, s20)
    val s211 = Eml(s21, s1)
    val s212 = Eml(s1, s21)
    val s213 = Eml(s21, s21)
    val s221 = Eml(s22, s1)
    val s222 = Eml(s1, s22)
    val s223 = Eml(s22, s22)
    println("s1: " + s1.debug)
    println("s20: " + s20.debug)
    println("s21: " + s21.debug)
    println("s201: " + s201.debug)
    println("s202: " + s202.debug)
    println("s203: " + s203.debug)
    println("s211: " + s211.debug)
    println("s212: " + s212.debug)
    println("s213: " + s213.debug)
    println("s221: " + s221.debug)
    println("s222: " + s222.debug)
    println("s223: " + s223.debug)
  }
}
