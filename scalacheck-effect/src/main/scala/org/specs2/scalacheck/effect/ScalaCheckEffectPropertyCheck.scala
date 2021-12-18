package org.specs2
package scalacheck
package effect

import cats.MonadThrow
import cats.syntax.all.*
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalacheck.Test
import org.scalacheck.effect.PropF
import org.scalacheck.rng.Seed
import org.scalacheck.util.FreqMap
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.*

import scala.util.control.NonFatal

import language.adhocExtensions
import execute.*
import matcher.*
import PrettyDetails.*

trait ScalaCheckEffectPropertyCheck extends ScalaCheckPropertyCheck:

  /** checks if the property is true for each generated value, and with the specified parameters
    */
  def check[F[_]: MonadThrow](
      prop: PropF[F],
      parameters: Parameters,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty
  ): F[Result] =

    val initialSeed = parameters.seed.getOrElse(Seed.random())

    val genParams = Gen.Parameters.default.withInitialSeed(initialSeed)

    prop.check(parameters.testParameters, genParams).map { result =>
      val prettyTestResult = prettyResult(result, parameters, initialSeed, prettyFreqMap)(parameters.prettyParams)
      val testResult = if parameters.prettyParams.verbosity == 0 then "" else prettyTestResult

      val checkResult =
        result match
          case Test.Result(Test.Passed, succeeded, discarded, fq, _) =>
            Success(prettyTestResult, testResult, succeeded)

          case Test.Result(Test.Proved(as), succeeded, discarded, fq, _) =>
            Success(prettyTestResult, testResult, succeeded)

          case Test.Result(Test.Exhausted, n, _, fq, _) =>
            Failure(prettyTestResult)

          case Test.Result(Test.Failed(args, labels), n, _, fq, _) =>
            new Failure(prettyTestResult, details = collectDetails(fq)) {
              // the location is already included in the failure message
              override def location = ""
            }

          case Test.Result(Test.PropException(args, ex, labels), n, _, fq, _) =>
            ex match
              case FailureException(f) =>
                // in that case we want to represent a normal failure
                val failedResult =
                  prettyResult(result.copy(status = Test.Failed(args, labels)), parameters, initialSeed, prettyFreqMap)(
                    parameters.prettyParams
                  )
                Failure(failedResult + "\n> " + f.message, details = f.details, trace = f.trace)

              case DecoratedResultException(DecoratedResult(_, f)) =>
                // in that case we want to represent a normal failure
                val failedResult =
                  prettyResult(result.copy(status = Test.Failed(args, labels)), parameters, initialSeed, prettyFreqMap)(
                    parameters.prettyParams
                  )
                f.setMessage(failedResult + "\n>\n" + f.message)

              case e: AssertionError =>
                val failedResult =
                  prettyResult(result.copy(status = Test.Failed(args, labels)), parameters, initialSeed, prettyFreqMap)(
                    parameters.prettyParams
                  )
                Failure(failedResult + "\n> " + e.getMessage, trace = e.getStackTrace.toList)

              case SkipException(s)    => s
              case PendingException(p) => p
              case NonFatal(t)         => Error(prettyTestResult + showCause(t), t)

      checkResultFailure(checkResult)

    }
