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

/** A ScalaCheckEffectFunction adds the possibility to select various typeclass instances for a given property:
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

case class ScalaCheckEffectFunction3[F[_]: MonadThrow, T1, T2, T3, R](
    execute: (T1, T2, T3) => F[R],
    argInstances1: ScalaCheckArgInstances[T1],
    argInstances2: ScalaCheckArgInstances[T2],
    argInstances3: ScalaCheckArgInstances[T3],
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F] {

  type SelfType = ScalaCheckEffectFunction3[F, T1, T2, T3, R]

  private given AsResult[R] = asResult
  private given Arbitrary[T1] = argInstances1.arbitrary
  private given Arbitrary[T2] = argInstances2.arbitrary
  private given Arbitrary[T3] = argInstances3.arbitrary
  private given (T1 => Pretty) = argInstances1.pretty
  private given (T2 => Pretty) = argInstances2.pretty
  private given (T3 => Pretty) = argInstances3.pretty

  lazy val propFunction = (t1: T1, t2: T2, t3: T3) => {
    lazy val executed = execute(t1, t2, t3)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(
      (t1: T1) =>
        makePropF(
          (t2: T2) => makePropF((t3: T3) => propFunction(t1, t2, t3), argInstances3.shrink, parameters),
          argInstances2.shrink,
          parameters
        ),
      argInstances1.shrink,
      parameters
    )

  def noShrink: SelfType = copy(
    argInstances1 = argInstances1.copy(shrink = None),
    argInstances2 = argInstances2.copy(shrink = None),
    argInstances3 = argInstances3.copy(shrink = None)
  )

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType =
    copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType =
    copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType =
    copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collectAllArgs(f1: T1 => Any, f2: T2 => Any, f3: T3 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3)

  def collectAll: SelfType =
    collect1.collect2.collect3

  def prepare(action: (T1, T2, T3) => (T1, T2, T3)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3) => execute.tupled(action(t1, t2, t3)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))
}

case class ScalaCheckEffectFunction4[F[_]: MonadThrow, T1, T2, T3, T4, R](
    execute: (T1, T2, T3, T4) => F[R],
    argInstances1: ScalaCheckArgInstances[T1],
    argInstances2: ScalaCheckArgInstances[T2],
    argInstances3: ScalaCheckArgInstances[T3],
    argInstances4: ScalaCheckArgInstances[T4],
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F] {

  type SelfType = ScalaCheckEffectFunction4[F, T1, T2, T3, T4, R]

  private given AsResult[R] = asResult
  private given Arbitrary[T1] = argInstances1.arbitrary
  private given Arbitrary[T2] = argInstances2.arbitrary
  private given Arbitrary[T3] = argInstances3.arbitrary
  private given Arbitrary[T4] = argInstances4.arbitrary
  private given (T1 => Pretty) = argInstances1.pretty
  private given (T2 => Pretty) = argInstances2.pretty
  private given (T3 => Pretty) = argInstances3.pretty
  private given (T4 => Pretty) = argInstances4.pretty

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4) => {
    lazy val executed = execute(t1, t2, t3, t4)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(
      (t1: T1) =>
        makePropF(
          (t2: T2) =>
            makePropF(
              (t3: T3) => makePropF((t4: T4) => propFunction(t1, t2, t3, t4), argInstances4.shrink, parameters),
              argInstances3.shrink,
              parameters
            ),
          argInstances2.shrink,
          parameters
        ),
      argInstances1.shrink,
      parameters
    )

  def noShrink: SelfType = copy(
    argInstances1 = argInstances1.copy(shrink = None),
    argInstances2 = argInstances2.copy(shrink = None),
    argInstances3 = argInstances3.copy(shrink = None),
    argInstances4 = argInstances4.copy(shrink = None)
  )

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty, p4: T4 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String, p4: T4 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType =
    copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType =
    copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType =
    copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType =
    copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collectAllArgs(f1: T1 => Any, f2: T2 => Any, f3: T3 => Any, f4: T4 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4

  def prepare(action: (T1, T2, T3, T4) => (T1, T2, T3, T4)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4) => execute.tupled(action(t1, t2, t3, t4)))

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))
}

case class ScalaCheckEffectFunction5[F[_]: MonadThrow, T1, T2, T3, T4, T5, R](
    execute: (T1, T2, T3, T4, T5) => F[R],
    argInstances1: ScalaCheckArgInstances[T1],
    argInstances2: ScalaCheckArgInstances[T2],
    argInstances3: ScalaCheckArgInstances[T3],
    argInstances4: ScalaCheckArgInstances[T4],
    argInstances5: ScalaCheckArgInstances[T5],
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F] {

  type SelfType = ScalaCheckEffectFunction5[F, T1, T2, T3, T4, T5, R]

  private given AsResult[R] = asResult
  private given Arbitrary[T1] = argInstances1.arbitrary
  private given Arbitrary[T2] = argInstances2.arbitrary
  private given Arbitrary[T3] = argInstances3.arbitrary
  private given Arbitrary[T4] = argInstances4.arbitrary
  private given Arbitrary[T5] = argInstances5.arbitrary
  private given (T1 => Pretty) = argInstances1.pretty
  private given (T2 => Pretty) = argInstances2.pretty
  private given (T3 => Pretty) = argInstances3.pretty
  private given (T4 => Pretty) = argInstances4.pretty
  private given (T5 => Pretty) = argInstances5.pretty

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => {
    lazy val executed = execute(t1, t2, t3, t4, t5)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(
      (t1: T1) =>
        makePropF(
          (t2: T2) =>
            makePropF(
              (t3: T3) =>
                makePropF(
                  (t4: T4) => makePropF((t5: T5) => propFunction(t1, t2, t3, t4, t5), argInstances5.shrink, parameters),
                  argInstances4.shrink,
                  parameters
                ),
              argInstances3.shrink,
              parameters
            ),
          argInstances2.shrink,
          parameters
        ),
      argInstances1.shrink,
      parameters
    )

  def noShrink: SelfType = copy(
    argInstances1 = argInstances1.copy(shrink = None),
    argInstances2 = argInstances2.copy(shrink = None),
    argInstances3 = argInstances3.copy(shrink = None),
    argInstances4 = argInstances4.copy(shrink = None),
    argInstances5 = argInstances5.copy(shrink = None)
  )

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitraries(
      a1: Arbitrary[T1],
      a2: Arbitrary[T2],
      a3: Arbitrary[T3],
      a4: Arbitrary[T4],
      a5: Arbitrary[T5]
  ): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4).setArbitrary5(a5)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4).setShrink5(s5)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty, p4: T4 => Pretty, p5: T5 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4).setPretty5(p5)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String, p4: T4 => String, p5: T5 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType =
    copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType =
    copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType =
    copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType =
    copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType =
    copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collectAllArgs(f1: T1 => Any, f2: T2 => Any, f3: T3 => Any, f4: T4 => Any, f5: T5 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4).collectArg5(f5)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5

  def prepare(action: (T1, T2, T3, T4, T5) => (T1, T2, T3, T4, T5)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => execute.tupled(action(t1, t2, t3, t4, t5)))

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))
}

case class ScalaCheckEffectFunction6[F[_]: MonadThrow, T1, T2, T3, T4, T5, T6, R](
    execute: (T1, T2, T3, T4, T5, T6) => F[R],
    argInstances1: ScalaCheckArgInstances[T1],
    argInstances2: ScalaCheckArgInstances[T2],
    argInstances3: ScalaCheckArgInstances[T3],
    argInstances4: ScalaCheckArgInstances[T4],
    argInstances5: ScalaCheckArgInstances[T5],
    argInstances6: ScalaCheckArgInstances[T6],
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F] {

  type SelfType = ScalaCheckEffectFunction6[F, T1, T2, T3, T4, T5, T6, R]

  private given AsResult[R] = asResult
  private given Arbitrary[T1] = argInstances1.arbitrary
  private given Arbitrary[T2] = argInstances2.arbitrary
  private given Arbitrary[T3] = argInstances3.arbitrary
  private given Arbitrary[T4] = argInstances4.arbitrary
  private given Arbitrary[T5] = argInstances5.arbitrary
  private given Arbitrary[T6] = argInstances6.arbitrary
  private given (T1 => Pretty) = argInstances1.pretty
  private given (T2 => Pretty) = argInstances2.pretty
  private given (T3 => Pretty) = argInstances3.pretty
  private given (T4 => Pretty) = argInstances4.pretty
  private given (T5 => Pretty) = argInstances5.pretty
  private given (T6 => Pretty) = argInstances6.pretty

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => {
    lazy val executed = execute(t1, t2, t3, t4, t5, t6)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(
      (t1: T1) =>
        makePropF(
          (t2: T2) =>
            makePropF(
              (t3: T3) =>
                makePropF(
                  (t4: T4) =>
                    makePropF(
                      (t5: T5) =>
                        makePropF((t6: T6) => propFunction(t1, t2, t3, t4, t5, t6), argInstances6.shrink, parameters),
                      argInstances5.shrink,
                      parameters
                    ),
                  argInstances4.shrink,
                  parameters
                ),
              argInstances3.shrink,
              parameters
            ),
          argInstances2.shrink,
          parameters
        ),
      argInstances1.shrink,
      parameters
    )

  def noShrink: SelfType = copy(
    argInstances1 = argInstances1.copy(shrink = None),
    argInstances2 = argInstances2.copy(shrink = None),
    argInstances3 = argInstances3.copy(shrink = None),
    argInstances4 = argInstances4.copy(shrink = None),
    argInstances5 = argInstances5.copy(shrink = None),
    argInstances6 = argInstances6.copy(shrink = None)
  )

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitrary6(a6: Arbitrary[T6]): SelfType = copy(argInstances6 = argInstances6.copy(arbitrary = a6))
  def setArbitraries(
      a1: Arbitrary[T1],
      a2: Arbitrary[T2],
      a3: Arbitrary[T3],
      a4: Arbitrary[T4],
      a5: Arbitrary[T5],
      a6: Arbitrary[T6]
  ): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4).setArbitrary5(a5).setArbitrary6(a6)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGen6(g6: Gen[T6]): SelfType = setArbitrary6(Arbitrary(g6))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5).setGen6(g6)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrink6(s6: Shrink[T6]): SelfType = copy(argInstances6 = argInstances6.copy(shrink = Some(s6)))
  def setShrinks(
      s1: Shrink[T1],
      s2: Shrink[T2],
      s3: Shrink[T3],
      s4: Shrink[T4],
      s5: Shrink[T5],
      s6: Shrink[T6]
  ): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4).setShrink5(s5).setShrink6(s6)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def setPretty6(p6: T6 => Pretty): SelfType = copy(argInstances6 = argInstances6.copy(pretty = p6))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))
  def pretty6(p6: T6 => String): SelfType = setPretty6((t6: T6) => Pretty(_ => p6(t6)))

  def setPretties(
      p1: T1 => Pretty,
      p2: T2 => Pretty,
      p3: T3 => Pretty,
      p4: T4 => Pretty,
      p5: T5 => Pretty,
      p6: T6 => Pretty
  ): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4).setPretty5(p5).setPretty6(p6)
  def pretties(
      p1: T1 => String,
      p2: T2 => String,
      p3: T3 => String,
      p4: T4 => String,
      p5: T5 => String,
      p6: T6 => String
  ): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5).pretty6(p6)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType =
    copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType =
    copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType =
    copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType =
    copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType =
    copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collectArg6(f: T6 => Any): SelfType =
    copy(argInstances6 = argInstances6.copy(collectors = argInstances6.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collect6: SelfType = collectArg6(_.toString)
  def collectAllArgs(
      f1: T1 => Any,
      f2: T2 => Any,
      f3: T3 => Any,
      f4: T4 => Any,
      f5: T5 => Any,
      f6: T6 => Any
  ): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4).collectArg5(f5).collectArg6(f6)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5.collect6

  def prepare(action: (T1, T2, T3, T4, T5, T6) => (T1, T2, T3, T4, T5, T6)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => execute.tupled(action(t1, t2, t3, t4, t5, t6)))

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))
}

case class ScalaCheckEffectFunction7[F[_]: MonadThrow, T1, T2, T3, T4, T5, T6, T7, R](
    execute: (T1, T2, T3, T4, T5, T6, T7) => F[R],
    argInstances1: ScalaCheckArgInstances[T1],
    argInstances2: ScalaCheckArgInstances[T2],
    argInstances3: ScalaCheckArgInstances[T3],
    argInstances4: ScalaCheckArgInstances[T4],
    argInstances5: ScalaCheckArgInstances[T5],
    argInstances6: ScalaCheckArgInstances[T6],
    argInstances7: ScalaCheckArgInstances[T7],
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F] {

  type SelfType = ScalaCheckEffectFunction7[F, T1, T2, T3, T4, T5, T6, T7, R]

  private given AsResult[R] = asResult
  private given Arbitrary[T1] = argInstances1.arbitrary
  private given Arbitrary[T2] = argInstances2.arbitrary
  private given Arbitrary[T3] = argInstances3.arbitrary
  private given Arbitrary[T4] = argInstances4.arbitrary
  private given Arbitrary[T5] = argInstances5.arbitrary
  private given Arbitrary[T6] = argInstances6.arbitrary
  private given Arbitrary[T7] = argInstances7.arbitrary
  private given (T1 => Pretty) = argInstances1.pretty
  private given (T2 => Pretty) = argInstances2.pretty
  private given (T3 => Pretty) = argInstances3.pretty
  private given (T4 => Pretty) = argInstances4.pretty
  private given (T5 => Pretty) = argInstances5.pretty
  private given (T6 => Pretty) = argInstances6.pretty
  private given (T7 => Pretty) = argInstances7.pretty

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => {
    lazy val executed = execute(t1, t2, t3, t4, t5, t6, t7)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(
      (t1: T1) =>
        makePropF(
          (t2: T2) =>
            makePropF(
              (t3: T3) =>
                makePropF(
                  (t4: T4) =>
                    makePropF(
                      (t5: T5) =>
                        makePropF(
                          (t6: T6) =>
                            makePropF(
                              (t7: T7) => propFunction(t1, t2, t3, t4, t5, t6, t7),
                              argInstances7.shrink,
                              parameters
                            ),
                          argInstances6.shrink,
                          parameters
                        ),
                      argInstances5.shrink,
                      parameters
                    ),
                  argInstances4.shrink,
                  parameters
                ),
              argInstances3.shrink,
              parameters
            ),
          argInstances2.shrink,
          parameters
        ),
      argInstances1.shrink,
      parameters
    )

  def noShrink: SelfType = copy(
    argInstances1 = argInstances1.copy(shrink = None),
    argInstances2 = argInstances2.copy(shrink = None),
    argInstances3 = argInstances3.copy(shrink = None),
    argInstances4 = argInstances4.copy(shrink = None),
    argInstances5 = argInstances5.copy(shrink = None),
    argInstances6 = argInstances6.copy(shrink = None),
    argInstances7 = argInstances7.copy(shrink = None)
  )

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitrary6(a6: Arbitrary[T6]): SelfType = copy(argInstances6 = argInstances6.copy(arbitrary = a6))
  def setArbitrary7(a7: Arbitrary[T7]): SelfType = copy(argInstances7 = argInstances7.copy(arbitrary = a7))
  def setArbitraries(
      a1: Arbitrary[T1],
      a2: Arbitrary[T2],
      a3: Arbitrary[T3],
      a4: Arbitrary[T4],
      a5: Arbitrary[T5],
      a6: Arbitrary[T6],
      a7: Arbitrary[T7]
  ): SelfType =
    setArbitrary1(a1)
      .setArbitrary2(a2)
      .setArbitrary3(a3)
      .setArbitrary4(a4)
      .setArbitrary5(a5)
      .setArbitrary6(a6)
      .setArbitrary7(a7)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGen6(g6: Gen[T6]): SelfType = setArbitrary6(Arbitrary(g6))
  def setGen7(g7: Gen[T7]): SelfType = setArbitrary7(Arbitrary(g7))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5).setGen6(g6).setGen7(g7)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrink6(s6: Shrink[T6]): SelfType = copy(argInstances6 = argInstances6.copy(shrink = Some(s6)))
  def setShrink7(s7: Shrink[T7]): SelfType = copy(argInstances7 = argInstances7.copy(shrink = Some(s7)))
  def setShrinks(
      s1: Shrink[T1],
      s2: Shrink[T2],
      s3: Shrink[T3],
      s4: Shrink[T4],
      s5: Shrink[T5],
      s6: Shrink[T6],
      s7: Shrink[T7]
  ): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4).setShrink5(s5).setShrink6(s6).setShrink7(s7)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def setPretty6(p6: T6 => Pretty): SelfType = copy(argInstances6 = argInstances6.copy(pretty = p6))
  def setPretty7(p7: T7 => Pretty): SelfType = copy(argInstances7 = argInstances7.copy(pretty = p7))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))
  def pretty6(p6: T6 => String): SelfType = setPretty6((t6: T6) => Pretty(_ => p6(t6)))
  def pretty7(p7: T7 => String): SelfType = setPretty7((t7: T7) => Pretty(_ => p7(t7)))

  def setPretties(
      p1: T1 => Pretty,
      p2: T2 => Pretty,
      p3: T3 => Pretty,
      p4: T4 => Pretty,
      p5: T5 => Pretty,
      p6: T6 => Pretty,
      p7: T7 => Pretty
  ): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4).setPretty5(p5).setPretty6(p6).setPretty7(p7)
  def pretties(
      p1: T1 => String,
      p2: T2 => String,
      p3: T3 => String,
      p4: T4 => String,
      p5: T5 => String,
      p6: T6 => String,
      p7: T7 => String
  ): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5).pretty6(p6).pretty7(p7)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType =
    copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType =
    copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType =
    copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType =
    copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType =
    copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collectArg6(f: T6 => Any): SelfType =
    copy(argInstances6 = argInstances6.copy(collectors = argInstances6.collectors :+ f))
  def collectArg7(f: T7 => Any): SelfType =
    copy(argInstances7 = argInstances7.copy(collectors = argInstances7.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collect6: SelfType = collectArg6(_.toString)
  def collect7: SelfType = collectArg7(_.toString)
  def collectAllArgs(
      f1: T1 => Any,
      f2: T2 => Any,
      f3: T3 => Any,
      f4: T4 => Any,
      f5: T5 => Any,
      f6: T6 => Any,
      f7: T7 => Any
  ): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4).collectArg5(f5).collectArg6(f6).collectArg7(f7)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5.collect6.collect7

  def prepare(action: (T1, T2, T3, T4, T5, T6, T7) => (T1, T2, T3, T4, T5, T6, T7)): SelfType =
    copy(execute =
      (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => execute.tupled(action(t1, t2, t3, t4, t5, t6, t7))
    )

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))
}

case class ScalaCheckEffectFunction8[F[_]: MonadThrow, T1, T2, T3, T4, T5, T6, T7, T8, R](
    execute: (T1, T2, T3, T4, T5, T6, T7, T8) => F[R],
    argInstances1: ScalaCheckArgInstances[T1],
    argInstances2: ScalaCheckArgInstances[T2],
    argInstances3: ScalaCheckArgInstances[T3],
    argInstances4: ScalaCheckArgInstances[T4],
    argInstances5: ScalaCheckArgInstances[T5],
    argInstances6: ScalaCheckArgInstances[T6],
    argInstances7: ScalaCheckArgInstances[T7],
    argInstances8: ScalaCheckArgInstances[T8],
    prettyFreqMap: FreqMap[Set[Any]] => Pretty,
    asResult: AsResult[R],
    context: Option[Context],
    parameters: Parameters
) extends ScalaCheckEffectFunction[F] {

  type SelfType = ScalaCheckEffectFunction8[F, T1, T2, T3, T4, T5, T6, T7, T8, R]

  private given AsResult[R] = asResult
  private given Arbitrary[T1] = argInstances1.arbitrary
  private given Arbitrary[T2] = argInstances2.arbitrary
  private given Arbitrary[T3] = argInstances3.arbitrary
  private given Arbitrary[T4] = argInstances4.arbitrary
  private given Arbitrary[T5] = argInstances5.arbitrary
  private given Arbitrary[T6] = argInstances6.arbitrary
  private given Arbitrary[T7] = argInstances7.arbitrary
  private given Arbitrary[T8] = argInstances8.arbitrary
  private given (T1 => Pretty) = argInstances1.pretty
  private given (T2 => Pretty) = argInstances2.pretty
  private given (T3 => Pretty) = argInstances3.pretty
  private given (T4 => Pretty) = argInstances4.pretty
  private given (T5 => Pretty) = argInstances5.pretty
  private given (T6 => Pretty) = argInstances6.pretty
  private given (T7 => Pretty) = argInstances7.pretty
  private given (T8 => Pretty) = argInstances8.pretty

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => {
    lazy val executed = execute(t1, t2, t3, t4, t5, t6, t7, t8)
    executed: PropF[F]
  }

  lazy val propF: PropF[F] =
    makePropF(
      (t1: T1) =>
        makePropF(
          (t2: T2) =>
            makePropF(
              (t3: T3) =>
                makePropF(
                  (t4: T4) =>
                    makePropF(
                      (t5: T5) =>
                        makePropF(
                          (t6: T6) =>
                            makePropF(
                              (t7: T7) =>
                                makePropF(
                                  (t8: T8) => propFunction(t1, t2, t3, t4, t5, t6, t7, t8),
                                  argInstances8.shrink,
                                  parameters
                                ),
                              argInstances7.shrink,
                              parameters
                            ),
                          argInstances6.shrink,
                          parameters
                        ),
                      argInstances5.shrink,
                      parameters
                    ),
                  argInstances4.shrink,
                  parameters
                ),
              argInstances3.shrink,
              parameters
            ),
          argInstances2.shrink,
          parameters
        ),
      argInstances1.shrink,
      parameters
    )

  def noShrink: SelfType = copy(
    argInstances1 = argInstances1.copy(shrink = None),
    argInstances2 = argInstances2.copy(shrink = None),
    argInstances3 = argInstances3.copy(shrink = None),
    argInstances4 = argInstances4.copy(shrink = None),
    argInstances5 = argInstances5.copy(shrink = None),
    argInstances6 = argInstances6.copy(shrink = None),
    argInstances7 = argInstances7.copy(shrink = None),
    argInstances8 = argInstances8.copy(shrink = None)
  )

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitrary6(a6: Arbitrary[T6]): SelfType = copy(argInstances6 = argInstances6.copy(arbitrary = a6))
  def setArbitrary7(a7: Arbitrary[T7]): SelfType = copy(argInstances7 = argInstances7.copy(arbitrary = a7))
  def setArbitrary8(a8: Arbitrary[T8]): SelfType = copy(argInstances8 = argInstances8.copy(arbitrary = a8))
  def setArbitraries(
      a1: Arbitrary[T1],
      a2: Arbitrary[T2],
      a3: Arbitrary[T3],
      a4: Arbitrary[T4],
      a5: Arbitrary[T5],
      a6: Arbitrary[T6],
      a7: Arbitrary[T7],
      a8: Arbitrary[T8]
  ): SelfType =
    setArbitrary1(a1)
      .setArbitrary2(a2)
      .setArbitrary3(a3)
      .setArbitrary4(a4)
      .setArbitrary5(a5)
      .setArbitrary6(a6)
      .setArbitrary7(a7)
      .setArbitrary8(a8)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGen6(g6: Gen[T6]): SelfType = setArbitrary6(Arbitrary(g6))
  def setGen7(g7: Gen[T7]): SelfType = setArbitrary7(Arbitrary(g7))
  def setGen8(g8: Gen[T8]): SelfType = setArbitrary8(Arbitrary(g8))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8])
      : SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5).setGen6(g6).setGen7(g7).setGen8(g8)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrink6(s6: Shrink[T6]): SelfType = copy(argInstances6 = argInstances6.copy(shrink = Some(s6)))
  def setShrink7(s7: Shrink[T7]): SelfType = copy(argInstances7 = argInstances7.copy(shrink = Some(s7)))
  def setShrink8(s8: Shrink[T8]): SelfType = copy(argInstances8 = argInstances8.copy(shrink = Some(s8)))
  def setShrinks(
      s1: Shrink[T1],
      s2: Shrink[T2],
      s3: Shrink[T3],
      s4: Shrink[T4],
      s5: Shrink[T5],
      s6: Shrink[T6],
      s7: Shrink[T7],
      s8: Shrink[T8]
  ): SelfType =
    setShrink1(s1)
      .setShrink2(s2)
      .setShrink3(s3)
      .setShrink4(s4)
      .setShrink5(s5)
      .setShrink6(s6)
      .setShrink7(s7)
      .setShrink8(s8)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def setPretty6(p6: T6 => Pretty): SelfType = copy(argInstances6 = argInstances6.copy(pretty = p6))
  def setPretty7(p7: T7 => Pretty): SelfType = copy(argInstances7 = argInstances7.copy(pretty = p7))
  def setPretty8(p8: T8 => Pretty): SelfType = copy(argInstances8 = argInstances8.copy(pretty = p8))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))
  def pretty6(p6: T6 => String): SelfType = setPretty6((t6: T6) => Pretty(_ => p6(t6)))
  def pretty7(p7: T7 => String): SelfType = setPretty7((t7: T7) => Pretty(_ => p7(t7)))
  def pretty8(p8: T8 => String): SelfType = setPretty8((t8: T8) => Pretty(_ => p8(t8)))

  def setPretties(
      p1: T1 => Pretty,
      p2: T2 => Pretty,
      p3: T3 => Pretty,
      p4: T4 => Pretty,
      p5: T5 => Pretty,
      p6: T6 => Pretty,
      p7: T7 => Pretty,
      p8: T8 => Pretty
  ): SelfType =
    setPretty1(p1)
      .setPretty2(p2)
      .setPretty3(p3)
      .setPretty4(p4)
      .setPretty5(p5)
      .setPretty6(p6)
      .setPretty7(p7)
      .setPretty8(p8)
  def pretties(
      p1: T1 => String,
      p2: T2 => String,
      p3: T3 => String,
      p4: T4 => String,
      p5: T5 => String,
      p6: T6 => String,
      p7: T7 => String,
      p8: T8 => String
  ): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5).pretty6(p6).pretty7(p7).pretty8(p8)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType =
    copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType =
    copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType =
    copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType =
    copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType =
    copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collectArg6(f: T6 => Any): SelfType =
    copy(argInstances6 = argInstances6.copy(collectors = argInstances6.collectors :+ f))
  def collectArg7(f: T7 => Any): SelfType =
    copy(argInstances7 = argInstances7.copy(collectors = argInstances7.collectors :+ f))
  def collectArg8(f: T8 => Any): SelfType =
    copy(argInstances8 = argInstances8.copy(collectors = argInstances8.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collect6: SelfType = collectArg6(_.toString)
  def collect7: SelfType = collectArg7(_.toString)
  def collect8: SelfType = collectArg8(_.toString)
  def collectAllArgs(
      f1: T1 => Any,
      f2: T2 => Any,
      f3: T3 => Any,
      f4: T4 => Any,
      f5: T5 => Any,
      f6: T6 => Any,
      f7: T7 => Any,
      f8: T8 => Any
  ): SelfType =
    collectArg1(f1)
      .collectArg2(f2)
      .collectArg3(f3)
      .collectArg4(f4)
      .collectArg5(f5)
      .collectArg6(f6)
      .collectArg7(f7)
      .collectArg8(f8)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5.collect6.collect7.collect8

  def prepare(action: (T1, T2, T3, T4, T5, T6, T7, T8) => (T1, T2, T3, T4, T5, T6, T7, T8)): SelfType =
    copy(execute =
      (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) =>
        execute.tupled(action(t1, t2, t3, t4, t5, t6, t7, t8))
    )

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))
}
