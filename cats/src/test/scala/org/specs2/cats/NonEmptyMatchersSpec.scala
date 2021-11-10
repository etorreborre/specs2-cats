package org.specs2.cats

import cats.data.*
import org.specs2.Specification

class NonEmptyMatchersSpec extends Specification with NonEmptyMatchers:
  def is = s2"""

  we can check the size of a NonEmptyList $nonempty1
  we can check the size of a NonEmptyChain $nonempty2
  we can check the size of a NonEmptyVector $nonempty3

  """

  def nonempty1 = NonEmptyList(1, List(2, 3)) must haveSize(3)
  def nonempty2 = NonEmptyChain(1, 2, 3) must haveSize(3)
  def nonempty3 = NonEmptyVector(1, Vector(2, 3)) must haveSize(3)
