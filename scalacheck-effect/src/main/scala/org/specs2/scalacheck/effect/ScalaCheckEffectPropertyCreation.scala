package org.specs2
package scalacheck
package effect

import cats.MonadThrow
import org.scalacheck.*
import org.scalacheck.util.FreqMap
import org.scalacheck.util.Pretty

import execute.AsResult
import ScalaCheckEffectProperty.*

trait ScalaCheckEffectPropertyCreation:

  /** create a ScalaCheck property from a function */
  def propF[F[_]: MonadThrow, T, R](result: T => F[R])(implicit
      arbitrary: Arbitrary[T],
      shrink: Shrink[T],
      pretty: T => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction1[F, T, R] =
    ScalaCheckEffectFunction1(
      result,
      arbitrary,
      Some(shrink),
      collectors = Nil,
      pretty,
      prettyFreqMap,
      asResult,
      context = None,
      parameters
    )

  /** create a ScalaCheck property from a function of 2 arguments */
  def propF[F[_]: MonadThrow, T1, T2, R](result: (T1, T2) => F[R])(using
      arbitrary1: Arbitrary[T1],
      shrink1: Shrink[T1],
      pretty1: T1 => Pretty,
      arbitrary2: Arbitrary[T2],
      shrink2: Shrink[T2],
      pretty2: T2 => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction2[F, T1, T2, R] =
    ScalaCheckEffectFunction2(
      result,
      ScalaCheckArgInstances(arbitrary1, Some(shrink1), collectors = Nil, pretty = pretty1),
      ScalaCheckArgInstances(arbitrary2, Some(shrink2), collectors = Nil, pretty = pretty2),
      prettyFreqMap = prettyFreqMap,
      asResult,
      context = None,
      parameters
    )
