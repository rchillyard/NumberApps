package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.expression.expr.Expression.em
import com.phasmidsoftware.number.expression.expr.ExpressionMatchers.componentsSimplifier
import com.phasmidsoftware.number.expression.expr.*

case class Eml(x: Expression, y: Expression) extends BinaryFunction(x, UniFunction(y, Negate), Sum) {

  override lazy val terms: Seq[Expression] = Seq(x, y)

  lazy val operandsMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Eml:operandsMatcher") {
      case x@HasEuler() =>
        em.Miss(s"Eml:simplifyOperands: Euler should not be simplified here: $x", this)
      case eml: Eml =>
        componentsSimplifier(terms, { xs => val Seq(newX, newY) = xs; Eml(newX, newY) })
    }

  /**
    * Simplifies a given `BiFunction` expression where trivial simplifications can be applied.
    * This method identifies patterns in the expression that represent mathematical identities
    * or redundancies and replaces them with their simplified form. Examples of such patterns
    * include removing identities, handling zero and one in products or powers, and other basic
    * algebraic simplifications.
    *
    * The simplifications include:
    * - Removing or simplifying identity elements (e.g., `x * 1 = x`, `x + 0 = x`).
    * - Handling special cases like zeroes in products or powers.
    * - Applying reductions for specific constants or algebraic terms.
    *
    * If no simplification can be applied, the method encapsulates this as a "miss" result.
    * Implementations of this method aim to provide lightweight simplification logic,
    * with more complex cases deferred to other parts of the system.
    *
    * @return an `em.AutoMatcher[Expression]` instance encapsulating the simplified `Expression`
    *         if simplification was possible, or a "miss" if no trivial simplification could be applied.
    */
  lazy val identitiesMatcher: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Eml:identitiesMatcher") {
      case Eml(One, One) =>
        em.Match(E)
      case Eml(x, One) =>
        em.Match(UniFunction(x, Exp))
      case _ =>
        em.Miss[Expression, Expression]("Eml: identitiesMatcher: no trivial simplifications", this)
    }

  /**
    * Simplifies a `CompositeExpression` represented as a `BiFunction` by applying various matchers
    * to identify opportunities for simplification. Specifically:
    * - Converts the `BiFunction` into an `Aggregate` for consistent simplification processing.
    * - Attempts to simplify complementary terms within the `BiFunction`.
    * - Applies additional simplification logic as defined by `Expression.structuralMatcher` and `matchSimpler`.
    *
    * TODO try to shorten this method.
    *
    * If the expression cannot be simplified, the result will indicate the failure.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying the `BiFunction`.
    *         It provides either the simplified `Expression` or indicates that no simplification was possible.
    */
  lazy val structuralMatcher: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Eml:structuralMatcher")(x =>
      em.Miss("structuralMatcher", x))

  def renderAsExpression: String = s"Eml(${x.render},${y.render})"
}

object Eml {
  val E: Eml = Eml(One, One)
}