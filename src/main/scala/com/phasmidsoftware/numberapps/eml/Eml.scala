package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.algebra.core.Renderable
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.expression.expr.*

sealed trait S extends Renderable:
  def exp: S = Eml(this, 1)

  def ln: S = Eml(1, Eml(Eml(1, this), 1))

  def expand: Seq[S]

  infix def +(s: S): S = (exp * s.exp).ln

  infix def *(s: S): S = (ln + s.ln).exp

  def asExpression: Expression

case object `1` extends S {
  def expand: Seq[S] = Seq(Eml.e)

  def asExpression: Expression = One

  def render: String = "1"
}

case class Eml(x: S, y: S) extends S:
  def expand: Seq[S] = this match {
    case Eml(`1`, `1`) => Seq(Eml(Eml.e, Eml.e))
    case Eml(`1`, z) => Seq(Eml(Eml.e, z))
    case Eml(z, `1`) => Seq(Eml(z, Eml.e))
  }

  def asExpression: Expression = rawExpression.simplify

  def render: String = s"eml($x,$y)"

  override def toString: String = render

  def debug: String = s"${rawExpression.toLatex}; $asExpression; ${asExpression.materialize}"

  private lazy val rawExpression =
    BiFunction(UniFunction(x.asExpression, Exp), UniFunction(UniFunction(y.asExpression, Ln), Negate), Sum)

object Eml {
  def apply(x: Int, y: S): S = x match {
    case 1 => new Eml(`1`, y)
    case _ => throw EmlException(s"apply($x,$y)")
  }

  def apply(x: S, y: Int): S = y match {
    case 1 => new Eml(x, `1`)
    case _ => throw EmlException(s"apply($x,$y)")
  }

  val one: S = `1`
  val zero: S = `1`.ln
  val e: S = `1`.exp
}

case class EmlException(str: String) extends Exception(str)