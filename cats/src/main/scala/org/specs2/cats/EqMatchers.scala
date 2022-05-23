package org.specs2.cats

import cats.kernel.Eq
import cats.implicits.*
import org.specs2.matcher.describe.*
import org.specs2.text.Indent.*

/** Matchers for the Eq datatype
  */
trait EqDiffable:
  given eqDiffable[T : Eq]: Diffable[T] =
    new Diffable[T]:
      def diff(actual: T, expected: T): ComparisonResult =
        new ComparisonResult:
          def identical: Boolean =
            actual.eqv(expected)

          def render: String =
            if identical then
              s"$actual == $expected"
            else
              s"$actual != $expected"

          override def render(indent: String): String =
            render.indentWith(indent)

object EqDiffable extends EqDiffable
