package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.Expression
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EmlFuncSpec extends AnyFlatSpec with Matchers
 {
  it should "grow 4" in {
    val ss = Eml.e.expandN(4)
    ss.size shouldBe 45_8329
    val es: Set[Expression] = ss.take(100).map(s => s.asExpression)
    val simple = es.filter(_.evaluateAsIs.isDefined)
    simple.size shouldBe 10
  }

   it should "perform task with expansion 3" in {
     val allTrees = One.expandN(3)
     allTrees.size shouldBe 26
     val exactTrees = allTrees
       .map(s => s -> s.asExpression)
       .filter(_._2.evaluateAsIs.isDefined)
       .toMap
     println(exactTrees.size)
     println(exactTrees)
   }

   it should "perform task with expansion 4 in parallel" in {
     import scala.collection.parallel.CollectionConverters.*

     val allTrees = One.expandN(4)
     allTrees.size shouldBe 677
     val exactTrees = allTrees.par
       .map(s => s -> s.asExpression)
       .filter(_._2.evaluateAsIs.isDefined)
       .toMap
     println(exactTrees.size)
     println(exactTrees)
   }
 }
