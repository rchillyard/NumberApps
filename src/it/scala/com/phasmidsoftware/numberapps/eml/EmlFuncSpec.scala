package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.{Expression, Noop}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EmlFuncSpec extends AnyFlatSpec with Matchers {

  it should "expandN(3) on e" in {
    import Eml.constants
    val ss = Eml.e.expandN(3)
    ss.size shouldBe 673
    println(s"Size of expanded set: ${ss.size}")
    val es: Seq[Expression] = ss.toSeq.zipWithIndex.map {
      (s, i) =>
        i match {
          case _ =>
            val expression = s.asExpression
            val evaluatedExpression = expression.evaluateAsIs
            if (evaluatedExpression.isDefined)
              System.err.println(s"Exact Expression: #$i: $s -> ${evaluatedExpression.get}")
            expression
        }
    }
    val exact = es.filter(_.evaluateAsIs.isDefined)
    //    exact.size shouldBe 7 // NOTE this was accurate before we made most `Infinity` operations yield `Infinity`.
    exact.size shouldBe 32
  }

  it should "expandN(4) on e" in {
    import Eml.constants
    val ss = Eml.e.expandN(4)
    ss.size shouldBe 458_285
    println(s"Size of expanded set: ${ss.size}")
    val es: Seq[Expression] = ss.toSeq.zipWithIndex.take(100_000).map {
      (s, i) =>
        i match {
          case 98 | 319 | 421 | 717 | 829 | 835 | 1132 | 1381 =>
            Noop(s"$i")
          case _ if i < 99_999 =>
            val expression = s.asExpression
            val evaluatedExpression = expression.evaluateAsIs
            if (evaluatedExpression.isDefined)
              System.err.println(s"Exact Expression: #$i: $s -> ${evaluatedExpression.get}")
            expression
          case _ =>
            System.err.print(s"Expression: #$i: $s -> ")
            val expression = s.asExpression
            System.err.println(s"$expression")
            expression
        }
    }
    val simple = es.filter(_.evaluateAsIs.isDefined)
    simple.size shouldBe 4332
  }

  it should "perform task with expansion 3 in parallel" in {
    import Eml.constants
    import scala.collection.parallel.CollectionConverters.*

    val allTrees = One.expandN(3)
    allTrees.size shouldBe 26
    val exactTrees = allTrees.par
      .map(s => s -> s.asExpression)
      .filter(_._2.evaluateAsIs.isDefined)
      .toMap
    println(exactTrees.size)
    println(exactTrees)
  }

  it should "perform task with expansion 4 in parallel" in {
    import Eml.constants

    import scala.collection.parallel.CollectionConverters.*
    val allTrees = One.expandN(4)
    allTrees.size shouldBe 674
    val exactTrees = allTrees.par
      .map(s => s -> s.asExpression)
      .filter(_._2.evaluateAsIs.isDefined)
      .toMap
    println(s"There are ${exactTrees.size} exact trees")
    println(exactTrees)
  }
}
