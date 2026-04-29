package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.algebra.core.Renderable
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.expression.expr.*

import scala.annotation.tailrec

/**
  * A sealed trait representing a mathematical structure that supports various operations
  * including exponentiation, natural logarithms, addition, and multiplication.
  *
  * This trait extends `Renderable`, enabling instances to be rendered as strings.
  * It allows for composition of mathematical expressions through the following methods:
  *
  * - `exp`: Computes the exponential of the current instance.
  * - `ln`: Computes the natural logarithm of the current instance.
  * - `expand`: Expands the current instance into a sequence of its components.
  * - `+`: Adds the current instance to another instance of `S`.
  * - `*`: Multiplies the current instance with another instance of `S`.
  * - `asExpression`: Converts the current instance to an `Expression`.
  */
sealed trait S extends Renderable:
  def exp: S = Eml(this, 1)

  def ln: S = Eml(1, Eml(Eml(1, this), 1))

  def expand: Set[S]

  def expandOnce: Set[S] = expand

  def expandN(rounds: Int): Set[S] =
    (1 to rounds).foldLeft(Set(this: S)) { (current, _) =>
      current.flatMap(_.expandOnce)
    }

  def expandNToMap[T, U](fKey: S => T)(fValue: S => U)(rounds: Int): Map[T, U] =
    (1 to rounds).foldLeft(Map(fKey(this) -> this)) { (current, _) =>
      current.values.flatMap(_.expandOnce)
        .map(s => fKey(s) -> s)
        .toMap
    }.map((k, v) => (k, fValue(v)))

  infix def +(s: S): S = (exp * s.exp).ln

  infix def *(s: S): S = (ln + s.ln).exp

  def asExpression: Expression

/**
  * Represents the mathematical value of one as a single instance and implements the behavior of the `S` trait.
  *
  * This object provides methods to:
  * - Retrieve the expanded form of the object as a sequence of `S`.
  * - Retrieve the expression form of this object.
  * - Render the object into its string representation.
  */
case object One extends S {
  def expand: Set[S] = Set(this, Eml(One, One))

  def asExpression: Expression = com.phasmidsoftware.number.expression.expr.One

  def render: String = "1"

  override def toString: String = render
}

/**
  * The `Eml` case class represents a composite structure within the hierarchy of `S`.
  * It extends the `S` trait and encapsulates two components, `x` and `y`, both of type `S`.
  *
  * This class provides the following capabilities:
  *
  * - Expanding the current instance into a sequence of `S` elements based on specific patterns.
  * - Representing the mathematical expression corresponding to the instance in a simplified form.
  * - Rendering a string representation of the instance.
  * - Enabling debugging output to inspect internal computations and associated expressions.
  *
  * Methods:
  * - `expand`: Expands the instance into a sequence of derived elements based on predefined matching rules.
  * - `asExpression`: Converts the instance into a simplified `Expression` representation.
  * - `render`: Returns a string representation in the form `eml(x, y)`.
  * - `toString`: Overrides the default `toString` method with the rendered representation.
  * - `debug`: Produces debugging information, including expressions in LaTeX format, simplified expressions,
  *   and materialized expressions for detailed examination.
  *
  * Internally, it lazily computes a raw mathematical expression based on its components
  * using specific functions (`UniFunction`, `BiFunction`, `Exp`, `Ln`, `Negate`, and `Sum`).
  */
case class Eml(x: S, y: S) extends S:
  def expand: Set[S] =
    for
      xExp <- x.expand
      yExp <- y.expand
    yield Eml(xExp, yExp)

  def asExpression: Expression = rawExpression.simplify

  def render: String = s"eml($x,$y)"

  override def toString: String = render

  /**
    * Generates a debug string representation of the current `Eml` instance.
    *
    * The string includes:
    * - The LaTeX representation of the raw expression associated with the `Eml` instance.
    * - The string representation of the `Eml` instance as an expression.
    * - The materialized form of the expression derived from the `Eml` instance.
    *
    * @return A concatenated string combining the LaTeX of the raw expression,
    *         the string representation of the expression, and its materialized form,
    *         separated by semicolons.
    */
  def debug: String = s"${rawExpression.toLatex}; $asExpression; ${asExpression.materialize}"

  private lazy val rawExpression =
    BiFunction(UniFunction(x.asExpression, Exp), UniFunction(UniFunction(y.asExpression, Ln), Negate), Sum)

/**
  * The `Eml` object acts as a factory for creating instances of the `Eml` class and provides
  * predefined constants and operations for working with mathematical expressions in the `S` structure.
  *
  * This object enables the construction of `Eml` instances through overloaded `apply` methods,
  * and it also defines key constants such as `one`, `zero`, and `e` for common mathematical values.
  */
object Eml {
  /**
    * Creates an instance of `S` based on the provided integer and `S` input parameters.
    *
    * This method constructs an `Eml` instance where the first parameter is matched to predefined cases.
    * If the integer input `x` equals 1, it returns a new `Eml` instance with `One` and the provided `y`.
    * Otherwise, it throws an `EmlException`.
    *
    * @param x an integer used to determine the structure of the resulting `S` instance.
    * @param y a value of type `S` that forms the second component of the resulting `Eml` instance.
    * @return an instance of `S`, specifically an `Eml` object if conditions are met.
    * @throws EmlException if `x` does not equal 1.
    */
  def apply(x: Int, y: S): S = x match {
    case 1 => new Eml(One, y)
    case _ => throw EmlException(s"apply($x,$y)")
  }

  /**
    * Applies a transformation to the input parameters and returns an instance of `S`.
    *
    * This method evaluates the integer parameter `y` to determine the operation performed on the `S`-typed input `x`.
    * If `y` equals 1, it constructs a new `Eml` instance using the provided `x` and a predefined `One` value.
    * For other values of `y`, an `EmlException` is thrown to indicate invalid input.
    *
    * @param x a value of type `S` that serves as one of the components of the resulting instance.
    * @param y an integer value used to determine the operation or structure of the result.
    * @return an instance of type `S`. If `y` equals 1, this is a newly constructed `Eml` object.
    * @throws EmlException if `y` is not equal to 1.
    */
  def apply(x: S, y: Int): S = y match {
    case 1 => new Eml(x, One)
    case _ => throw EmlException(s"apply($x,$y)")
  }

  /**
    * Applies a transformation to the input integer parameters and returns an instance of `S`.
    *
    * This method evaluates the first integer parameter `x` to determine the behavior of the application.
    * If `x` is equal to 1, a secondary `apply` method is invoked with predefined values (`One` and `y`).
    * For other values of `x`, an `EmlException` is thrown.
    *
    * @param x an integer used to determine the case for constructing or handling the resulting `S` instance.
    * @param y an integer representing another component or parameter for the resulting instance.
    * @return an instance of type `S` constructed based on `x` and `y`.
    * @throws EmlException if `x` does not match predefined conditions (e.g., `x` not equal to 1).
    */
  def apply(x: Int, y: Int): S = x match {
    case 1 => apply(One, y)
    case _ => throw EmlException(s"apply($x,$y)")
  }
  //
  //  def grow(n: Int): Set[S] =
  //    @tailrec
  //    def inner(r: Set[S])(ss: Set[S], i: Int): Set[S] = (ss, i) match {
  //      case (_, -1) =>
  //        r
  //      case (ss, _) if ss.isEmpty =>
  //        r
  //      case (q: Set[S], 0) =>
  //        inner(r ++ q)(Set.empty, i - 1)
  //      case (q: Set[S], _) =>
  //        val ss1 = q.flatMap(_.expand)
  //        inner(r ++ q)(ss1, i - 1)
  //    }
  //    inner(Set.empty)(Set(One), n)

  val one: S = One
  val zero: S = One.ln
  val e: S = One.exp
}

case class EmlException(str: String) extends Exception(str)