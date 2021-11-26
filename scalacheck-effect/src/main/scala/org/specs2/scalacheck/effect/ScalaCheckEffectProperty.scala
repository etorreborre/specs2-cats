package org.specs2
package scalacheck
package effect

import cats.MonadThrow
import org.scalacheck.*
import org.scalacheck.effect.PropF
import org.scalacheck.rng.Seed
import org.scalacheck.util.FreqMap
import org.scalacheck.util.Pretty

import execute.*
import specification.*
import core.{AsExecution, Execution}
import AsResultPropF.{given, *}
import ScalaCheckEffectProperty.{given, *}

/** A ScalaCheckProperty encapsulates a ScalaCheck Prop and its parameters
  */
trait ScalaCheckEffectProperty[F[_]]:
  type SelfType <: ScalaCheckEffectProperty[F]

  def propF: PropF[F]

  def parameters: Parameters

  def prettyFreqMap: FreqMap[Set[Any]] => Pretty

  def setParameters(ps: Parameters): SelfType

  def setSeed(seed: Seed): SelfType

  def setSeed(seed: String): SelfType

  def setVerbosity(v: Int): SelfType =
    setParameters(parameters.setVerbosity(v))

  def set(
      minTestsOk: Int = parameters.minTestsOk,
      minSize: Int = parameters.minSize,
      maxDiscardRatio: Float = parameters.maxDiscardRatio,
      maxSize: Int = parameters.maxSize,
      workers: Int = parameters.workers,
      callback: Test.TestCallback = parameters.testCallback,
      loader: Option[ClassLoader] = parameters.loader
  ): SelfType =
    setParameters(
      parameters.copy(
        minTestsOk = minTestsOk,
        minSize = minSize,
        maxDiscardRatio = maxDiscardRatio,
        maxSize = maxSize,
        workers = workers,
        testCallback = callback,
        loader = loader
      )
    )

  def display(
      minTestsOk: Int = parameters.minTestsOk,
      minSize: Int = parameters.minSize,
      maxDiscardRatio: Float = parameters.maxDiscardRatio,
      maxSize: Int = parameters.maxSize,
      workers: Int = parameters.workers,
      callback: Test.TestCallback = parameters.testCallback,
      loader: Option[ClassLoader] = parameters.loader
  ): SelfType =
    setParameters(
      parameters
        .copy(
          minTestsOk = minTestsOk,
          minSize = minSize,
          maxDiscardRatio = maxDiscardRatio,
          maxSize = maxSize,
          workers = workers,
          testCallback = callback,
          loader = loader
        )
        .setVerbosity(1)
    )

  def verbose: SelfType =
    setVerbosity(1)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType

  def prettyFreqMap(f: FreqMap[Set[Any]] => String): SelfType =
    setPrettyFreqMap((fq: FreqMap[Set[Any]]) => Pretty(_ => f(fq)))

object ScalaCheckEffectProperty:

  given ScalaCheckEffectPropertyAsExecution[F[_]: MonadThrow, S <: ScalaCheckEffectProperty[F]](using
      AsExecution[F[Result]]
  ): AsExecution[S] with
    def execute(s: =>S): Execution =
      Execution.withEnvFlatten { env =>
        AsResultPropF.check(s.propF, s.parameters.overrideWith(env.commandLine), s.prettyFreqMap)
      }

  def makePropF[F[_]: MonadThrow, T](f: T => PropF[F], shrink: Option[Shrink[T]], parameters: Parameters)(implicit
      a: Arbitrary[T],
      p: T => Pretty
  ): PropF[F] =
    shrink match
      case Some(s) => PropF.forAllShrinkF(a.arbitrary, s.shrink)(f)
      case _       => PropF.forAllNoShrinkF(f)

/** A ScalaCheckFunction adds the possibility to select various typeclass instances for a given property:
  *
  *   - Arbitrary to generate values
  *   - Shrink to shrink counter-examples
  *   - Show to display arguments in case of a counter-example
  *   - Collector to collect values and provide a summary as string (to show frequencies for example)
  */
trait ScalaCheckEffectFunction[F[_]] extends ScalaCheckEffectProperty[F]:
  def noShrink: SelfType

  def context: Option[Context]

  def setContext(context: Context): SelfType

  def before(action: =>Any): SelfType =
    setContext(Before.create(action))

  def after(action: =>Any): SelfType =
    setContext(After.create(action))

  def beforeAfter(beforeAction: =>Any, afterAction: =>Any): SelfType =
    setContext(BeforeAfter.create(beforeAction, afterAction))

  def around(action: Result => Result): SelfType =
    setContext(Around.create(action))

  protected def executeInContext[R: AsResult](result: =>R) = {
    lazy val executed = result
    context.foreach(_(executed))
    executed.asInstanceOf[Matchable] match {
      case p: Prop => p
      case other   => AsResultProp.asResultToProp.apply(other.asInstanceOf[R])
    }
  }

case class ScalaCheckEffectFunction1[F[_]: MonadThrow, T, R](
    execute: T => F[R],
    arbitrary: Arbitrary[T],
    shrink: Option[Shrink[T]],
    collectors: List[T => Any],
    pretty: T => Pretty,
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F]:

  type SelfType = ScalaCheckEffectFunction1[F, T, R]

  private given asResult1: AsResult[R] = asResult
  private given arbitrary1: Arbitrary[T] = arbitrary
  private given pretty1: (T => Pretty) = pretty

  lazy val propFFunction = (t: T) => {
    lazy val executed = execute(t)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(propFFunction, shrink, parameters)

  def noShrink: SelfType = copy(shrink = None, pretty = pretty1)

  def setArbitrary(arbitrary: Arbitrary[T]): SelfType =
    copy(arbitrary = arbitrary, pretty = pretty1)

  def setGen(gen: Gen[T]): SelfType =
    setArbitrary(Arbitrary(gen))

  def setShrink(shrink: Shrink[T]): SelfType =
    copy(shrink = Some(shrink), pretty = pretty1)

  def setPretty(pretty: T => Pretty): SelfType =
    copy(pretty = pretty1)

  def pretty(pretty: T => String): SelfType =
    setPretty((t: T) => Pretty(_ => pretty(t)))

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f, pretty = pretty1)

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps, pretty = pretty1)

  def collect: SelfType =
    collectArg(_.toString)

  def collectArg(f: T => Any): SelfType =
    copy(collectors = collectors :+ f, pretty = pretty1)

  def prepare(action: T => T): SelfType =
    copy(execute = (t: T) => execute(action(t)), pretty = pretty)

  def setContext(context: Context): SelfType =
    copy(context = Some(context), pretty = pretty)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)), pretty = pretty1)

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)), pretty = pretty1)

case class ScalaCheckEffectFunction2[F[_]: MonadThrow, T1, T2, R](
    execute: (T1, T2) => F[R],
    argInstances1: ScalaCheckArgInstances[T1],
    argInstances2: ScalaCheckArgInstances[T2],
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F] {

  type SelfType = ScalaCheckEffectFunction2[F, T1, T2, R]

  private given AsResult[R] = asResult
  private given Arbitrary[T1] = argInstances1.arbitrary
  private given Arbitrary[T2] = argInstances2.arbitrary
  private given (T1 => Pretty) = argInstances1.pretty
  private given (T2 => Pretty) = argInstances2.pretty

  lazy val propFFunction = (t1: T1, t2: T2) => {
    lazy val executed = execute(t1, t2)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(
      (t1: T1) => makePropF((t2: T2) => propFFunction(t1, t2), argInstances2.shrink, parameters),
      argInstances1.shrink,
      parameters
    )

  def noShrink: SelfType =
    copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGens(g1: Gen[T1], g2: Gen[T2]): SelfType =
    setGen1(g1).setGen2(g2)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2]): SelfType =
    setShrink1(s1).setShrink2(s2)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2)
  def pretties(p1: T1 => String, p2: T2 => String): SelfType =
    pretty1(p1).pretty2(p2)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType =
    copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType =
    copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collectAllArgs(f1: T1 => Any, f2: T2 => Any): SelfType =
    collectArg1(f1).collectArg2(f2)

  def collectAll: SelfType =
    collect1.collect2

  def prepare(action: (T1, T2) => (T1, T2)): SelfType =
    copy(execute = (t1: T1, t2: T2) => execute.tupled(action(t1, t2)))

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))
}
