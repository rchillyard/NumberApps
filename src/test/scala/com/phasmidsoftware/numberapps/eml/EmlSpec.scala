package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.{E, Infinity, One, Zero}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EmlSpec extends AnyFlatSpec with Matchers {

  behavior of "Eml"

  it should "handle E" in {
    val e = Eml.E
    e.renderAsExpression shouldBe "Eml(1,1)"
    e.simplify shouldBe E
  }
  it should "handle Eml(0,1)" in {
    val e = Eml(0, 1)
    e.renderAsExpression shouldBe "Eml(0,1)"
    e.simplify shouldBe One
  }
  it should "handle Eml(0,0)" in {
    val e = Eml(-Infinity, 1)
    e.renderAsExpression shouldBe "Eml(-(∞),1)"
    e.simplify shouldBe Zero
  }
}
