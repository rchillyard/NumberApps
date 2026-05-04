package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.Expression
import com.phasmidsoftware.numberapps.eml.Eml.findExactTreesPar
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
    exact.size shouldBe 32
  }

  it should "expandN(4) on e" in {
    import Eml.constants
    val ss = Eml.e.expandN(4)
    ss.size shouldBe 458_285
    println(s"Size of expanded set: ${ss.size}")
    val es: Set[Expression] = ss.map {
      s =>
        val expression = s.asExpression
        val evaluatedExpression = expression.evaluateAsIs
        if (evaluatedExpression.isDefined)
          System.err.println(s"Exact Expression: $s -> ${evaluatedExpression.get}")
        expression
    }
    val simple = es.filter(_.evaluateAsIs.isDefined)
//    simple.size shouldBe 15348   ????
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

  it should "perform task with expansion 5 in parallel" in {
    val (allTrees, exactTrees) = findExactTreesPar
    allTrees shouldBe 458_285
    exactTrees.size shouldBe 49
    exactTrees.foreach {
      case (k, v) => println(s"Exact tree: $k -> $v")
    }
  }

  it should "expandN(5) on 1 with debug behavior" in {
    import Eml.constants
    val ss = One.expandN(5)
    ss.size shouldBe 458_285
    val drop = 343_710
    val take = 400_994 - 343_710 + 1
    println(s"Size of expanded set: ${ss.size}")
    val es: Seq[Expression] = ss.toSeq.zipWithIndex.slice(drop, drop + take).map {
      (s, i) =>
        i match {
          // Ignore the sequence number for now...
          case _ =>
            System.err.print(s"Expression: #$i: $s (${s.asInstanceOf[Eml].rawExpression})-> ")
            val expression = s.asExpression
            val maybeEager = expression.evaluateAsIs
            if (maybeEager.isDefined)
              System.err.println(s"Evaluated: ${maybeEager.get}")
            else
              System.err.println(s"Expression: $expression")
            expression
        }
    }
    es.size shouldBe 57285
  }

}
