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

  /** create a ScalaCheck property from a function of 3 arguments */
  def propF[F[_]: MonadThrow, T1, T2, T3, R](result: (T1, T2, T3) => F[R])(using
      arbitrary1: Arbitrary[T1],
      shrink1: Shrink[T1],
      pretty1: T1 => Pretty,
      arbitrary2: Arbitrary[T2],
      shrink2: Shrink[T2],
      pretty2: T2 => Pretty,
      arbitrary3: Arbitrary[T3],
      shrink3: Shrink[T3],
      pretty3: T3 => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction3[F, T1, T2, T3, R] =
    ScalaCheckEffectFunction3(
      result,
      ScalaCheckArgInstances(arbitrary1, Some(shrink1), collectors = Nil, pretty = pretty1),
      ScalaCheckArgInstances(arbitrary2, Some(shrink2), collectors = Nil, pretty = pretty2),
      ScalaCheckArgInstances(arbitrary3, Some(shrink3), collectors = Nil, pretty = pretty3),
      prettyFreqMap = prettyFreqMap,
      asResult,
      context = None,
      parameters
    )

  /** create a ScalaCheck property from a function of 4 arguments */
  def propF[F[_]: MonadThrow, T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => F[R])(using
      arbitrary1: Arbitrary[T1],
      shrink1: Shrink[T1],
      pretty1: T1 => Pretty,
      arbitrary2: Arbitrary[T2],
      shrink2: Shrink[T2],
      pretty2: T2 => Pretty,
      arbitrary3: Arbitrary[T3],
      shrink3: Shrink[T3],
      pretty3: T3 => Pretty,
      arbitrary4: Arbitrary[T4],
      shrink4: Shrink[T4],
      pretty4: T4 => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction4[F, T1, T2, T3, T4, R] =
    ScalaCheckEffectFunction4(
      result,
      ScalaCheckArgInstances(arbitrary1, Some(shrink1), collectors = Nil, pretty = pretty1),
      ScalaCheckArgInstances(arbitrary2, Some(shrink2), collectors = Nil, pretty = pretty2),
      ScalaCheckArgInstances(arbitrary3, Some(shrink3), collectors = Nil, pretty = pretty3),
      ScalaCheckArgInstances(arbitrary4, Some(shrink4), collectors = Nil, pretty = pretty4),
      prettyFreqMap = prettyFreqMap,
      asResult,
      context = None,
      parameters
    )

  /** create a ScalaCheck property from a function of 5 arguments */
  def propF[F[_]: MonadThrow, T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => F[R])(using
      arbitrary1: Arbitrary[T1],
      shrink1: Shrink[T1],
      pretty1: T1 => Pretty,
      arbitrary2: Arbitrary[T2],
      shrink2: Shrink[T2],
      pretty2: T2 => Pretty,
      arbitrary3: Arbitrary[T3],
      shrink3: Shrink[T3],
      pretty3: T3 => Pretty,
      arbitrary4: Arbitrary[T4],
      shrink4: Shrink[T4],
      pretty4: T4 => Pretty,
      arbitrary5: Arbitrary[T5],
      shrink5: Shrink[T5],
      pretty5: T5 => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction5[F, T1, T2, T3, T4, T5, R] =
    ScalaCheckEffectFunction5(
      result,
      ScalaCheckArgInstances(arbitrary1, Some(shrink1), collectors = Nil, pretty = pretty1),
      ScalaCheckArgInstances(arbitrary2, Some(shrink2), collectors = Nil, pretty = pretty2),
      ScalaCheckArgInstances(arbitrary3, Some(shrink3), collectors = Nil, pretty = pretty3),
      ScalaCheckArgInstances(arbitrary4, Some(shrink4), collectors = Nil, pretty = pretty4),
      ScalaCheckArgInstances(arbitrary5, Some(shrink5), collectors = Nil, pretty = pretty5),
      prettyFreqMap = prettyFreqMap,
      asResult,
      context = None,
      parameters
    )

  /** create a ScalaCheck property from a function of 6 arguments */
  def propF[F[_]: MonadThrow, T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => F[R])(using
      arbitrary1: Arbitrary[T1],
      shrink1: Shrink[T1],
      pretty1: T1 => Pretty,
      arbitrary2: Arbitrary[T2],
      shrink2: Shrink[T2],
      pretty2: T2 => Pretty,
      arbitrary3: Arbitrary[T3],
      shrink3: Shrink[T3],
      pretty3: T3 => Pretty,
      arbitrary4: Arbitrary[T4],
      shrink4: Shrink[T4],
      pretty4: T4 => Pretty,
      arbitrary5: Arbitrary[T5],
      shrink5: Shrink[T5],
      pretty5: T5 => Pretty,
      arbitrary6: Arbitrary[T6],
      shrink6: Shrink[T6],
      pretty6: T6 => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction6[F, T1, T2, T3, T4, T5, T6, R] =
    ScalaCheckEffectFunction6(
      result,
      ScalaCheckArgInstances(arbitrary1, Some(shrink1), collectors = Nil, pretty = pretty1),
      ScalaCheckArgInstances(arbitrary2, Some(shrink2), collectors = Nil, pretty = pretty2),
      ScalaCheckArgInstances(arbitrary3, Some(shrink3), collectors = Nil, pretty = pretty3),
      ScalaCheckArgInstances(arbitrary4, Some(shrink4), collectors = Nil, pretty = pretty4),
      ScalaCheckArgInstances(arbitrary5, Some(shrink5), collectors = Nil, pretty = pretty5),
      ScalaCheckArgInstances(arbitrary6, Some(shrink6), collectors = Nil, pretty = pretty6),
      prettyFreqMap = prettyFreqMap,
      asResult,
      context = None,
      parameters
    )

  /** create a ScalaCheck property from a function of 7 arguments */
  def propF[F[_]: MonadThrow, T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => F[R])(using
      arbitrary1: Arbitrary[T1],
      shrink1: Shrink[T1],
      pretty1: T1 => Pretty,
      arbitrary2: Arbitrary[T2],
      shrink2: Shrink[T2],
      pretty2: T2 => Pretty,
      arbitrary3: Arbitrary[T3],
      shrink3: Shrink[T3],
      pretty3: T3 => Pretty,
      arbitrary4: Arbitrary[T4],
      shrink4: Shrink[T4],
      pretty4: T4 => Pretty,
      arbitrary5: Arbitrary[T5],
      shrink5: Shrink[T5],
      pretty5: T5 => Pretty,
      arbitrary6: Arbitrary[T6],
      shrink6: Shrink[T6],
      pretty6: T6 => Pretty,
      arbitrary7: Arbitrary[T7],
      shrink7: Shrink[T7],
      pretty7: T7 => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction7[F, T1, T2, T3, T4, T5, T6, T7, R] =
    ScalaCheckEffectFunction7(
      result,
      ScalaCheckArgInstances(arbitrary1, Some(shrink1), collectors = Nil, pretty = pretty1),
      ScalaCheckArgInstances(arbitrary2, Some(shrink2), collectors = Nil, pretty = pretty2),
      ScalaCheckArgInstances(arbitrary3, Some(shrink3), collectors = Nil, pretty = pretty3),
      ScalaCheckArgInstances(arbitrary4, Some(shrink4), collectors = Nil, pretty = pretty4),
      ScalaCheckArgInstances(arbitrary5, Some(shrink5), collectors = Nil, pretty = pretty5),
      ScalaCheckArgInstances(arbitrary6, Some(shrink6), collectors = Nil, pretty = pretty6),
      ScalaCheckArgInstances(arbitrary7, Some(shrink7), collectors = Nil, pretty = pretty7),
      prettyFreqMap = prettyFreqMap,
      asResult,
      context = None,
      parameters
    )

  /** create a ScalaCheck property from a function of 8 arguments */
  def propF[F[_]: MonadThrow, T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => F[R])(using
      arbitrary1: Arbitrary[T1],
      shrink1: Shrink[T1],
      pretty1: T1 => Pretty,
      arbitrary2: Arbitrary[T2],
      shrink2: Shrink[T2],
      pretty2: T2 => Pretty,
      arbitrary3: Arbitrary[T3],
      shrink3: Shrink[T3],
      pretty3: T3 => Pretty,
      arbitrary4: Arbitrary[T4],
      shrink4: Shrink[T4],
      pretty4: T4 => Pretty,
      arbitrary5: Arbitrary[T5],
      shrink5: Shrink[T5],
      pretty5: T5 => Pretty,
      arbitrary6: Arbitrary[T6],
      shrink6: Shrink[T6],
      pretty6: T6 => Pretty,
      arbitrary7: Arbitrary[T7],
      shrink7: Shrink[T7],
      pretty7: T7 => Pretty,
      arbitrary8: Arbitrary[T8],
      shrink8: Shrink[T8],
      pretty8: T8 => Pretty,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty,
      asResult: AsResult[R],
      parameters: Parameters
  ): ScalaCheckEffectFunction8[F, T1, T2, T3, T4, T5, T6, T7, T8, R] =
    ScalaCheckEffectFunction8(
      result,
      ScalaCheckArgInstances(arbitrary1, Some(shrink1), collectors = Nil, pretty = pretty1),
      ScalaCheckArgInstances(arbitrary2, Some(shrink2), collectors = Nil, pretty = pretty2),
      ScalaCheckArgInstances(arbitrary3, Some(shrink3), collectors = Nil, pretty = pretty3),
      ScalaCheckArgInstances(arbitrary4, Some(shrink4), collectors = Nil, pretty = pretty4),
      ScalaCheckArgInstances(arbitrary5, Some(shrink5), collectors = Nil, pretty = pretty5),
      ScalaCheckArgInstances(arbitrary6, Some(shrink6), collectors = Nil, pretty = pretty6),
      ScalaCheckArgInstances(arbitrary7, Some(shrink7), collectors = Nil, pretty = pretty7),
      ScalaCheckArgInstances(arbitrary8, Some(shrink8), collectors = Nil, pretty = pretty8),
      prettyFreqMap = prettyFreqMap,
      asResult,
      context = None,
      parameters
    )
