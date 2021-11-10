package org.specs2.cats

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import org.specs2.Specification

class ValidatedMatchersSpec extends Specification with ValidatedMatchers:
  def is = s2"""

  we can check that a Validated result is valid $validated1
  we can check that a Validated result is invalid $validated2

  """

  def validated1 = Valid(10) must beValid(beGreaterThan(5))
  def validated2 = Invalid("Wrong") must beInvalid(startWith("W"))
