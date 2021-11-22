package org.specs2
package scalacheck
package effect

import org.scalacheck.*
import org.scalacheck.effect.PropF
import org.scalacheck.rng.Seed
import org.scalacheck.util.FreqMap
import org.scalacheck.util.Pretty

import execute.*
import specification.*
import core.{AsExecution, Execution}
import AsResultProp.{given, *}
import ScalaCheckProperty.{given, *}

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

