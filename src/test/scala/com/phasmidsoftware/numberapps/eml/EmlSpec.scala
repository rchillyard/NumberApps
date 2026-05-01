package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.{E, Expression, Infinity, MinusOne, Zero, One as ExprOne}
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
    e.asExpression shouldBe Infinity
  }
  it should "expand One 1" in {
    val expand: Set[S] = One.expand
    expand shouldBe Set(One, Eml.e)
  }
  it should "expand Eml.e 2" in {
    val expand: Set[S] = Eml.e.expand
    expand shouldBe Set(
      Eml(1, 1),
      Eml(Eml(1, 1), Eml(1, 1)),
      Eml(1, Eml(1, 1)),
      Eml(Eml(1, 1), 1))
  }
  it should "expand Eml.e.expand 3" in {
    val expansion: Set[S] = Eml.e.expand
    println(expansion.mkString(", "))
    expansion shouldBe Set(
      Eml(1, 1),
      Eml(Eml(1, 1), Eml(1, 1)),
      Eml(1, Eml(1, 1)),
      Eml(Eml(1, 1), 1),
      Eml(Eml(1, 1), Eml(1, 1))
    )
  }
  it should "expand Eml.e.expand 4" in {
    val expansion: Set[S] = Eml.e.expand
    println(expansion.mkString(", "))
    expansion shouldBe Set(
      Eml(1, 1),
      Eml(Eml(1, 1), Eml(1, 1)),
      Eml(1, Eml(1, 1)),
      Eml(Eml(1, 1), 1),
      Eml(Eml(1, 1), Eml(1, 1))
    )
  }
  it should "grow 1" in {
    import Eml.constants
    val ss = Eml.e.expandN(1)
    ss.size shouldBe 4
    ss shouldBe Set(
      Eml(1, 1),
      Eml(1, Eml(1, 1)),
      Eml(Eml(1, 1), 1),
      Eml(Eml(1, 1), Eml(1, 1)))
  }
  it should "grow 2" in {
    import Eml.constants
    val ss = Eml.e.expandN(2)
    ss.size shouldBe 25
    ss shouldBe Set(
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)),
      Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), 1),
      Eml(Eml(1, Eml(1, 1)), 1),
      Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(1, 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))),
      Eml(Eml(1, 1), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), 1),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(1, 1), 1),
      Eml(1, 1),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(1, Eml(Eml(1, 1), Eml(1, 1))),
      Eml(1, Eml(Eml(1, 1), 1)),
      Eml(Eml(1, 1), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(1, 1)),
      Eml(1, Eml(1, Eml(1, 1))))
  }
  it should "grow 3" in {
    import Eml.constants
    val ss = Eml.e.expandN(3)
    ss.size shouldBe 673
    ss shouldBe Set(
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)),
      Eml(Eml(1, 1), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), 1),
      Eml(1, Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(1, Eml(1, 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), 1),
      Eml(Eml(1, 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(1, 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), 1),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), 1),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), 1),
      Eml(1, Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(1, Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, 1), 1),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(1, 1),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, 1)), 1),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), 1),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(1, Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), 1),
      Eml(Eml(1, 1), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), 1),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), 1),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(1, Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(1, Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), 1),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(1, Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(1, 1), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), 1),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), 1),
      Eml(1, Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(1, Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, 1), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(1, Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(1, 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(1, 1)),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(1, 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), 1), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(1, Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(1, Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(1, Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(1, 1), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(1, Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(1, Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(1, 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, 1), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(1, Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), 1), 1),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(1, Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, 1)),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(1, 1)),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), 1),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, 1)),
      Eml(1, Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(1, Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(1, Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(1, Eml(Eml(Eml(1, 1), 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), 1), Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), Eml(1, Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(1, Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), 1),
      Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1)))),
      Eml(Eml(1, 1), Eml(1, 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))),
      Eml(Eml(1, Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, 1), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), 1), 1)),
      Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), Eml(1, 1))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(1, 1), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)),
      Eml(Eml(1, Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))),
      Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1)))))
  }
  it should "grow 2 with String dedup" in {
    val ss = Eml.e.expandNToMap(_.asExpression.materialize.render)(s => (s -> s.asExpression))(2)
    ss.size shouldBe 25
    println(ss.mkString("\n"))
  }
  it should "grow 3 with non-dedup" in {
    val ss = Eml.e.expandNToMap(identity)(identity)(3)
    ss.size shouldBe 676
  }
  it should "grow and materialize 1" in {
    import Eml.constants
    val ss = Eml.e.expandN(1)
    val strings = ss map (s => s.asExpression.materialize.render)
    strings shouldBe Set("e", "1.718281828459045*", "15.154262241479259[11]", "14.154262241479259[11]")
  }
  it should "grow and materialize 2" in {
    import Eml.constants
    val strings = Eml.e.expandN(2) map (s => s.asExpression.materialize.render)
    strings shouldBe Set("e", "1.403192147249262[15]E+06", "3.814278563435342(24)E+06", "2.1769569738461270(10)", "1.403193865531090[15]E+06", "4.5749415247608790[39]", "0.0682660312478771[28]", "5.5749415247608790[39]", "2.9249257275497110[36]", "15.154262241479259[11]", "14.6129373868663400(62)", "0", "1.4031943242062361(87)E+06", "3.814278104760197[41]E+06", "3.814276386478369[41]E+06", "1.718281828459045*", "2.8566596963018340[22]", "14.154262241479259[11]", "12.5042464442680910[68]", "1.403194865531090[15]E+06", "3.814279104760197[41]E+06", "3.814276454744400[41]E+06", "5.0336166701479610(24)", "1.403192215515293[15]E+06", "12.435980413020214[11]")
  }
  it should "check the constants" in {
    for ((e, c) <- Eml.constants) {
      val expressionE = e.asExpression
      val expressionC = c.asExpression
      expressionE shouldBe expressionC
    }
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

  it should "materialize #97" in {
    val s = Eml(Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(1, Eml(1, 1)), 1)), Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), 1))
    val expression = s.asExpression
  }
  it should "materialize #319" in {
    val s = Eml(Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), 1))
    val expression = s.asExpression
  }
  it should "materialize #421" in {
    val s = Eml(Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1))), Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), 1))
    val expression = s.asExpression
  }
  it should "materialize #717" in {
    val s = Eml(Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))
    val expression = s.asExpression
  }
  it should "materialize #829" in {
    val s = Eml(Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1))), Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), 1))
    val expression = s.asExpression
  }
  it should "materialize #835" in {
    val s = Eml(Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, Eml(1, 1))), Eml(1, Eml(Eml(1, 1), Eml(1, 1)))), Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, Eml(1, 1))), 1))
    println(s.rawExpression)
    val expression = s.asExpression
  }
  it should "materialize #1132" in {
    val s = Eml(Eml(1, Eml(1, Eml(Eml(1, 1), 1))), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))
    println(s.rawExpression)
    val expression = s.asExpression
  }
  it should "materialize #1381" in {
    val s = Eml(Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, 1), 1))), Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), 1))
    val expression = s.asExpression
  }
}
