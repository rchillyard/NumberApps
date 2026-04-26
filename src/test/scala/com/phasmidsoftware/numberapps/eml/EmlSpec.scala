package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.{E, One, Zero}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EmlSpec extends AnyFlatSpec with Matchers {

  behavior of "Eml"

  it should "handle one" in {
    val one: S = Eml.one
    one.render shouldBe "1"
    one.asExpression shouldBe One
  }
  it should "handle E" in {
    val e: S = Eml.e
    e.render shouldBe "eml(1,1)"
    e.asExpression shouldBe E
  }
  it should "handle zero" in {
    val zero = Eml.zero
    zero.render shouldBe "eml(1,eml(eml(1,1),1))"
    zero.asExpression shouldBe Zero
  }
}
