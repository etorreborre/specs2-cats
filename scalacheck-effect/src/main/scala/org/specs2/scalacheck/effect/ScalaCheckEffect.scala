package org.specs2.scalacheck
package effect

trait ScalaCheckEffect
    extends ScalaCheckEffectPropertyCreation
    with ScalaCheckEffectPropertyCheck
    with ScalaCheckParameters
    with AsResultPropF
    with ScalaCheckEffectPropertyDsl
    with GenInstances
