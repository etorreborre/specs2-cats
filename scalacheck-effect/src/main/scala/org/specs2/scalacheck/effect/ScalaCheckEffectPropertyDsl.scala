package org.specs2
package scalacheck
package effect

import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalacheck.effect.PropF
import org.scalacheck.rng.Seed
import org.scalacheck.util.*
import org.specs2.specification.core.Fragments
import org.specs2.specification.create.FragmentsFactory

trait ScalaCheckEffectPropertyDsl extends FragmentsFactory with AsResultPropF:

  given propFToScalaCheckProperty[F[_]](using
      parameters: Parameters,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty
  ): Conversion[PropF[F], ScalaCheckEffectProp[F]] with
    def apply(propF: PropF[F]): ScalaCheckEffectProp[F] =
      ScalaCheckEffectProp(propF, parameters, prettyFreqMap)

case class ScalaCheckEffectProp[F[_]](
    propF: PropF[F],
    parameters: Parameters,
    prettyFreqMap: FreqMap[Set[Any]] => Pretty
) extends ScalaCheckEffectProperty[F]:
  type SelfType = ScalaCheckEffectProp[F]

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)
