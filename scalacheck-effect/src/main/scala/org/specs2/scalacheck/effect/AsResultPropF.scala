package org.specs2
package scalacheck
package effect

import cats.MonadThrow
import cats.syntax.all.*
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

  given asResultToPropF[F[_]: MonadThrow, R: AsResult]: Conversion[F[R], PropF[F]] with
    def apply(fr: F[R]): PropF[F] =
      PropF.effectOfPropFToPropF {
        MonadThrow[F].map(fr) { (r: R) =>
          val prop = AsResultProp.asResultToProp.apply(r)
          PropF { p =>
            val Prop.Result(status, args, collected, labels) = prop(p)
            PropF.Result(status, args, collected, labels)
          }
        }
      }

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
