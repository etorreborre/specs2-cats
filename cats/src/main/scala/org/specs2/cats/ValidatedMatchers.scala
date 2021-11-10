package org.specs2.cats

import cats.data.Validated
import org.specs2.matcher.OptionLikeCheckedMatcher
import org.specs2.matcher.OptionLikeMatcher
import org.specs2.matcher.ValueCheck

/** Matchers for the Validated datatype
  */
trait ValidatedMatchers:
  def beValid[T](t: ValueCheck[T]) = ValidValidatedCheckedMatcher(t)
  def beValid[T] = ValidValidatedMatcher[T]()

  def beInvalid[T](t: ValueCheck[T]) = InvalidValidatedCheckedMatcher(t)
  def beInvalid[T] = InvalidValidatedMatcher[T]()

object ValidatedMatchers extends ValidatedMatchers

case class ValidValidatedMatcher[T]() extends OptionLikeMatcher[Validated[?, T], T]("Valid", _.toOption)
case class ValidValidatedCheckedMatcher[T](check: ValueCheck[T])
    extends OptionLikeCheckedMatcher[Validated[?, T], T]("Valid", _.toEither.toOption, check)

case class InvalidValidatedMatcher[T]()
    extends OptionLikeMatcher[Validated[T, ?], T]("Invalid", _.toEither.left.toOption)
case class InvalidValidatedCheckedMatcher[T](check: ValueCheck[T])
    extends OptionLikeCheckedMatcher[Validated[T, ?], T]("Invalid", _.toEither.left.toOption, check)
