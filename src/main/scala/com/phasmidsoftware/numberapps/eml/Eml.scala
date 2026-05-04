package com.phasmidsoftware.numberapps.eml

import com.phasmidsoftware.number.algebra.core.Renderable
import com.phasmidsoftware.number.algebra.eager.{Eager, IsFinite, NaturalExponential, WholeNumber}
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.numberapps.eml.Eml.{findExactTreesFuture, logger}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.parallel.ParSeq
import scala.concurrent.duration.{Duration, DurationInt}

/**
  * A sealed trait representing a Tree whose grammar is simple:
  * `S ::= 1 | EML(S,S)`
  *
  * Please see the paper: [[https://arxiv.org/pdf/2603.21852v2 Odrzywolek (2026): All elementary functions from a single operator]].
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
  /**
    * Yields `e^this`.
    *
    * @return Returns an instance of type `S`, which is a result of applying
    *         the exponential operation to the current instance.
    */
  def exp: S = Eml(this, 1)

  /**
    * Computes the natural logarithm of the current instance.
    *
    * This method applies a specific transformation to the object it is invoked on,
    * representing the natural logarithm in a structured form.
    *
    * @return The result of the natural logarithm transformation, represented as an instance of type `S`.
    */
  def ln: S = Eml(1, Eml(Eml(1, this), 1))

  /**
    * Expands this instance by replacing any terminal occurrences of `1` by `Eml(1,1)`.
    *
    * @return a set of elements of type S resulting from the expansion process
    */
  def expand: Set[S]

  /**
    * Iteratively expands this S tree for the given number of rounds,
    * utilizing a provided mapping of known states which will be substituted if found.
    *
    * If a state is found in the `known` mapping, it is replaced with its corresponding value,
    * and further expansion halts for that state.
    *
    * @param rounds the number of iterations to perform the expansion
    * @param known  a mapping of states to their precomputed expanded forms
    * @return a set of states resulting from the defined expansion process
    */
  def expandN(rounds: Int)(using known: Map[S, S]): Set[S] =
    (1 to rounds).foldLeft(Set(this: S)) { (current, _) =>
      current.flatMap {
        case s if known.contains(s) => Set(known(s)) // leaf — don't expand further
        case s => s.expand
      }
    }

  /**
    * Transforms a starting instance of type `S` into a map of keys and values by repeatedly expanding it over a specified number of rounds.
    *
    * @param fKey   A function to extract a key of type `T` from an instance of type `S`.
    * @param fValue A function to extract a value of type `U` from an instance of type `S`.
    * @param rounds The number of expansion rounds to perform.
    * @return A map where the keys are derived using the `fKey` function and the values using the `fValue` function.
    */
  def expandNToMap[T, U](fKey: S => T)(fValue: S => U)(rounds: Int): Map[T, U] =
    (1 to rounds).foldLeft(Map(fKey(this) -> this)) { (current, _) =>
      current.values.flatMap(_.expand)
        .map(s => fKey(s) -> s)
        .toMap
    }.map((k, v) => (k, fValue(v)))

  /**
    * Adds the current instance of type `S` to another instance of type `S`.
    *
    * The method computes the sum by taking the natural logarithm of the product of the exponential
    * values of both instances.
    *
    * @param s another instance of type `S` to be added to the current instance.
    * @return an instance of type `S` representing the result of the addition.
    */
  infix def +(s: S): S = (exp * s.exp).ln

  /**
    * Multiplies the current instance of type `S` with another instance of type `S`.
    *
    * The method computes the product by taking the natural logarithm of both instances,
    * summing them, and then applying the exponential operation to the result.
    *
    * @param s another instance of type `S` to be multiplied with the current instance.
    * @return an instance of type `S` representing the result of the multiplication.
    */
  infix def *(s: S): S = (ln + s.ln).exp

  /**
    * Converts the current object into an instance of `Expression`.
    * This method is used to represent the object in the lazy expression type of `Number`.
    *
    * @return An instance of `Expression` representing the current EML tree.
    */
  def asExpression: Expression

object S {

  given Ordering[S] with
    def compare(x: S, y: S): Int = (x, y) match
      case (One, One) => 0
      case (One, _: Eml) => -1
      case (_: Eml, One) => 1
      case (Eml(x1, y1), Eml(x2, y2)) =>
        val c = compare(x1, x2)
        if c != 0 then c else compare(y1, y2)

}
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

  def asExpression: Expression = Eml.expressionCache.getOrElseUpdate(this, rawExpression.simplify)

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

  /**
    * A lazily evaluated instance of `Expression` withot any simplification.
    *
    * The structure of the raw expression is defined using:
    * - `BiFunction` to combine two transformed sub-expressions via a summation operation (`Sum`).
    * - The first sub-expression is created using `UniFunction` to apply an exponential transformation (`Exp`)
    *   to the result of invoking the `asExpression` method on `x`.
    * - The second sub-expression uses nested `UniFunction` calls:
    *   - The first `UniFunction` applies a logarithmic transformation (`Ln`) on the result of `y.asExpression`.
    *   - The result of this transformation is further negated via another `UniFunction` that applies the `Negate` operation.
    *
    * This expression serves as the foundation for representing and manipulating mathematical operations
    * within the `Eml` class.
    */
  lazy val rawExpression =
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

  private val expressionCache = scala.collection.concurrent.TrieMap[Eml, Expression]()

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def findExactTreesPar: (Int, ParSeq[(S, Eager)]) = {
    import Eml.constants
    import com.phasmidsoftware.number.core.numerical.Divides.given

    import scala.collection.parallel.CollectionConverters.*

    val allTrees = One.expandN(5)
    logger.info(s"Confirmed trees: ${allTrees.size}")
    val exactTrees = allTrees.par.zipWithIndex
      .map { case (s, i) => (s, i, s.asExpression.evaluateAsIs) }
      .map { case (k, i, v) => if 10_000 |> i then logger.info(s"Processing tree $i: $k"); (k, i, v) }
      .collect { case (k, _, Some(IsFinite(v))) => k -> v }
      .toMap
    (allTrees.size, exactTrees.toSeq)
  }

  def findExactTreesFuture(trees: Seq[S], perPartitionTimeout: Duration, overallTimeout: Duration, numPartitions: Int = 8): (Int, Seq[(S, Eager)]) = {
    import scala.concurrent.*
    import scala.concurrent.ExecutionContext.Implicits.global

    val allTrees = trees
    logger.info(s"Confirmed trees: ${allTrees.size}")
    val partitions = allTrees.zipWithIndex.grouped(allTrees.size / numPartitions).toSeq
    val indexedPartitions = partitions.zipWithIndex
    val futures: Seq[Future[Seq[(S, Eager)]]] =
      indexedPartitions.map {
        case (xs, i) =>
          val partition = Partition(xs, i)
          val willBeEvaluatedPartition = Future {
            val result = partition.evaluate
            logger.info(s"Partition ${partition} completed successfully")
            result
          }
          val willBeTimeout = Future {
            Thread.sleep(perPartitionTimeout.toMillis)
            throw new java.util.concurrent.TimeoutException(s"$partition) timed out after $perPartitionTimeout")
          }
          Future.firstCompletedOf(Seq(willBeEvaluatedPartition, willBeTimeout))
            .recover { case ex =>
              logger.info(s"Partition ${xs.head} failed or timed out: $ex")
              Seq.empty
            }
      }

    def flattenToMap(treeResultsByFuture: Seq[Seq[(S, Eager)]]): Map[S, Eager] =
      treeResultsByFuture.flatten.toMap

    val exactTrees: Map[S, Eager] =
      Await.result(
        Future.sequence(futures).map(flattenToMap),
        overallTimeout
      )

    (allTrees.size, exactTrees.toSeq)
  }

  val ProgressReportInterval = 2_000

  import com.phasmidsoftware.number.core.numerical.Divides.IntDivides

  def reportProgress(partitionIndex: Int, index: Int, tree: S): Unit =
    if ProgressReportInterval |> index then
      logger.info(s"Partition $partitionIndex, tree $index: $tree")

  def evaluateFiniteTree(tree: S): Option[(S, Eager)] =
    tree.asExpression.evaluateAsIs match
      case Some(IsFinite(value)) => Some(tree -> value)
      case _ => None

  case class Partition(partition: Seq[(S, Int)], partitionIndex: Int, start: Int):
    def size: Int = partition.size

    def evaluate: Seq[(S, Eager)] =
      partition.flatMap {
        case (tree, treeIndex) =>
          reportProgress(partitionIndex, treeIndex, tree)
          evaluateFiniteTree(tree)
      }

    override def toString: String = s"Partition: $partitionIndex, with size $size, starting with $start"

  object Partition:
    def apply(partition: Seq[(S, Int)], partitionIndex: Int): Partition =
      new Partition(partition, partitionIndex, partition.headOption.map(_._2).getOrElse(0))

  /**
    * A given set of constant mappings between instances of type `S`.
    *
    * This value represents predefined relations between specific structured instances
    * of the `S` trait. It provides a mapping that can be useful for lookup, transformation,
    * or substitution in computations involving the `S` grammar.
    *
    * @return A `Map[S, S]` containing the defined mappings of specific instances of `S`.
    */
  given constants: Map[S, S] = Map(
    Eml(1, Eml(1, Eml(Eml(1, Eml(1, 1)), 1))) -> e,
    Eml(1, Eml(Eml(1, 1), 1)) -> zero,
    Eml(1, Eml(Eml(1, Eml(1, 1)), 1)) -> One,
    Eml(1, Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))) -> e,
    Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), 1)) -> zero,
    Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)) -> One,
    Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(Eml(1, 1), 1)), 1)) -> e,
    Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, Eml(1, 1)), 1), 1)) -> zero,
    Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, Eml(1, 1)), Eml(1, 1)), 1)) -> One,
    Eml(Eml(1, Eml(1, 1)), Eml(Eml(Eml(1, Eml(1, 1)), Eml(Eml(1, 1), 1)), 1)) -> e,
    Eml(Eml(1, Eml(Eml(1, 1), 1)), 1) -> One,
    Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, 1)) -> zero,
    Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, Eml(Eml(1, Eml(1, 1)), 1))) -> One,
    Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))) -> One,
    Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(Eml(1, Eml(Eml(1, 1), 1)), 1)) -> One,
    Eml(Eml(1, Eml(Eml(1, Eml(1, 1)), 1)), Eml(1, Eml(Eml(1, Eml(1, 1)), 1))) -> e,
    Eml(Eml(1, Eml(Eml(1, Eml(1, 1)), 1)), Eml(Eml(1, 1), 1)) -> zero,
    Eml(Eml(1, Eml(Eml(1, Eml(1, 1)), 1)), Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))) -> e,
    Eml(Eml(1, Eml(Eml(1, Eml(1, 1)), 1)), Eml(Eml(1, Eml(1, 1)), 1)) -> One,
    Eml(Eml(1, Eml(Eml(1, Eml(1, 1)), 1)), Eml(Eml(1, Eml(Eml(1, 1), 1)), 1)) -> e,
    Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(Eml(1, 1), 1), 1), 1)) -> zero,
    Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(Eml(1, 1), 1), Eml(1, 1)), 1)) -> One,
    Eml(Eml(Eml(1, 1), 1), Eml(Eml(Eml(Eml(1, 1), 1), Eml(Eml(1, 1), 1)), 1)) -> e,
    Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), 1), 1)) -> zero,
    Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(1, 1)), 1)) -> One,
    Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(Eml(Eml(1, 1), Eml(1, 1)), Eml(Eml(1, 1), 1)), 1)) -> e,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), 1)), 1) -> One,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), 1)), Eml(1, 1)) -> zero,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), 1)), Eml(1, Eml(Eml(1, Eml(1, 1)), 1))) -> One,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), 1)), Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))) -> One,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), 1), 1)), Eml(Eml(1, Eml(Eml(1, 1), 1)), 1)) -> One,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), 1) -> e,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), Eml(1, Eml(Eml(1, Eml(1, 1)), 1))) -> e,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), Eml(Eml(1, 1), 1)) -> zero,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))) -> e,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), Eml(Eml(1, Eml(1, 1)), 1)) -> One,
    Eml(Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1)), Eml(Eml(1, Eml(Eml(1, 1), 1)), 1)) -> e,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), 1), 1) -> e,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), 1), Eml(1, Eml(Eml(1, Eml(1, 1)), 1))) -> e,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), 1), Eml(Eml(1, 1), 1)) -> zero,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), 1), Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))) -> e,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), 1), Eml(Eml(1, Eml(1, 1)), 1)) -> One,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), 1), Eml(Eml(1, Eml(Eml(1, 1), 1)), 1)) -> e,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, 1)), 1) -> One,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, 1)), Eml(1, 1)) -> zero,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, 1)), Eml(1, Eml(Eml(1, Eml(1, 1)), 1))) -> One,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, 1)), Eml(Eml(1, 1), Eml(Eml(Eml(1, 1), Eml(1, 1)), 1))) -> One,
    Eml(Eml(Eml(1, Eml(Eml(1, 1), 1)), Eml(1, 1)), Eml(Eml(1, Eml(Eml(1, 1), 1)), 1)) -> One)

  /**
    * A constant value of type `S` representing 1.
    */
  val one: S = One
  /**
    * Represents a predefined instance of type `S` corresponding to 0.
    *
    * This value is constructed by applying the `ln` method to the `One` instance, 
    * yielding the natural logarithm of `One` represented in the `S` grammar.
    */
  val zero: S = One.ln
  /**
    * Represents a predefined instance of type `S` corresponding to Euler's number `e`.
    *
    * This value is an instance of type `S`, constructed using the `exp` method on `One`.
    */
  val e: S = One.exp
}

@main def run(): Unit = {
  import Eml.constants

  import scala.concurrent.duration.*

  val (nAllTrees, exactTrees) = findExactTreesFuture(One.expandN(5).toSeq, 20.seconds, 5.minutes)
  exactTrees.sortBy(_._1).foreach { case (k, v) =>
    println(s"  $k -> ${
      v match
        case WholeNumber(1) => "One"
        case WholeNumber(0) => "zero"
        case NaturalExponential(WholeNumber(1)) => "e"
        case _ => v.toString
    },")
  }
}

@main def runQuick(): Unit = {
  import Eml.constants

  import scala.concurrent.duration.*

  val (nAllTrees, exactTrees) = findExactTreesFuture(One.expandN(4).toSeq, 2.minutes, 5.minutes, 4)
  exactTrees.sortBy(_._1).foreach { case (k, v) =>
    println(s"  $k -> ${
      v match
        case WholeNumber(1) => "One"
        case WholeNumber(0) => "zero"
        case NaturalExponential(WholeNumber(1)) => "e"
        case _ => v.toString
    },")
  }
}

@main def runA(): Unit = {
  import Eml.constants
  import com.phasmidsoftware.number.core.numerical.Divides.given

  val sample = One.expandN(5).toSeq.zipWithIndex
    .slice(408_000, 458_285)
    .filter { case (_, i) => 100 |> i }

  logger.info(s"Processing ${sample.size} trees")
  sample.foreach { case (tree, i) =>
    val t0 = System.currentTimeMillis()
    tree.asExpression.evaluateAsIs
    val elapsed = System.currentTimeMillis() - t0
    logger.info(s"Tree $i: ${elapsed}ms")
  }
  logger.info(s"Processed trees")
}

/**
  * Represents a custom exception used within the context of `Eml` operations.
  *
  * This exception is typically thrown in scenarios where input parameters
  * do not satisfy the expected preconditions for creating or transforming
  * instances of `S`. It includes a descriptive message encapsulated in the
  * `str` parameter to provide details about the error condition.
  *
  * @param str A descriptive error message indicating the cause of the exception.
  */
case class EmlException(str: String) extends Exception(str)