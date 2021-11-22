package org.specs2
package scalacheck
package effect

import cats.MonadThrow
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalacheck.effect.PropF
import org.scalacheck.util.*
import org.specs2.specification.core.AsExecution
import org.specs2.specification.core.Execution

import scala.annotation.tailrec

import execute.*

/** Implicits to convert PropF to AsResult and AsResult to PropF
  */
trait AsResultPropF extends ScalaCheckEffectPropertyCheck with AsResultPropFLowImplicits:

  given asResultToPropF[F[_]: MonadThrow, R: AsResult]: Conversion[R, PropF[F]] with
    def apply(r: R): PropF[F] =
      r.asInstanceOf[Matchable] match
        case p: PropF[F] @unchecked => p
        case _ =>
          lazy val result = ResultExecution.execute(AsResult(r))

          @tailrec
          def resultToProp(r: execute.Result): PropF[F] =
            r match
              case f: execute.Failure => PropF.exception(new FailureException(f))
              case s: execute.Skipped => PropF.exception(new SkipException(s))
              case p: execute.Pending => PropF.exception(new PendingException(p))
              case e: execute.Error   => PropF.exception(e.exception)

              case execute.DecoratedResult(_, r1) =>
                // display the datatables on a new line
                resultToProp(r1.updateMessage("\n" + _))

              case other => PropF.passed

          val prop = resultToProp(result)

          prop

  /** implicit typeclass instance to create examples from a Prop */
  given propFAsExecution[F[_]: MonadThrow](using p: Parameters, pfq: FreqMap[Set[Any]] => Pretty)(using
      exec: AsExecution[F[Result]]
  ): AsExecution[PropF[F]] with
    def execute(propF: =>PropF[F]): Execution =
      check(propF, p, pfq)

trait AsResultPropFLowImplicits extends ScalaCheckEffectPropertyCheck with ScalaCheckParameters:

  given scalaCheckPropertyAsExecution[F[_]: MonadThrow, S <: ScalaCheckEffectProperty[F]](using
      exec: AsExecution[F[Result]]
  ): AsExecution[S] with
    def execute(prop: =>S): Execution =
      try
        lazy val p = prop
        check(p.propF, p.parameters, p.prettyFreqMap)
      catch
        // this is necessary in case of thrown expectations inside the property
        case FailureException(f) =>
          f

        case t: Throwable =>
          AsResultPropF.propFAsExecution.execute(PropF.exception(t))

object AsResultPropF extends AsResultPropF
