package org.specs2
package matcher

import cats.effect.SyncIO
import execute.ResultImplicits.*
import text.NotNullStrings.*
import StringMatchers.{given, *}
import ValueChecks.{given}

import scala.language.adhocExtensions
import scala.reflect.ClassTag

trait SyncIOMatchers extends ValueChecks:
  def beOk[T]: SyncIOSuccessMatcher[T] =
    SyncIOSuccessMatcher()

  def beOk[T](check: ValueCheck[T]): SyncIOSuccessCheckedMatcher[T] =
    SyncIOSuccessCheckedMatcher(check)

  def beKo[T]: SyncIOFailureMatcher[T] =
    SyncIOFailureMatcher[T]()

  def beKo[T](check: ValueCheck[Throwable]): SyncIOFailureCheckedMatcher[T] =
    SyncIOFailureCheckedMatcher[T](check)

case class SyncIOSuccessMatcher[T]()
    extends OptionLikeMatcher[SyncIO[T], T]("a Success", (_: SyncIO[T]).attempt.unsafeRunSync().toOption):
  def withValue(t: ValueCheck[T]) = SyncIOSuccessCheckedMatcher(t)

case class SyncIOSuccessCheckedMatcher[T](check: ValueCheck[T])
    extends OptionLikeCheckedMatcher[SyncIO[T], T]("a Success", (_: SyncIO[T]).attempt.unsafeRunSync().toOption, check)

case class SyncIOFailureMatcher[T]()
    extends OptionLikeMatcher[SyncIO[T], Throwable]("a Failure", (_: SyncIO[T]).attempt.unsafeRunSync().left.toOption):
  def withValue(t: ValueCheck[Throwable]) = SyncIOFailureCheckedMatcher(t)

  def withThrowable[E <: Throwable: ClassTag] = SyncIOFailureCheckedMatcher[T]({ (t: Throwable) =>
    Expectations.createExpectable(t).applyMatcher(AnyMatchers.beAnInstanceOf[E])
  })

  def withThrowable[E <: Throwable: ClassTag](pattern: String) = SyncIOFailureCheckedMatcher[T]({ (t: Throwable) =>
    (Expectations.createExpectable(t).applyMatcher(AnyMatchers.beAnInstanceOf[E]) and
      Expectations.createExpectable(t.getMessage.notNull).applyMatcher(StringMatchers.beMatching(pattern)))
  })
case class SyncIOFailureCheckedMatcher[T](check: ValueCheck[Throwable])
    extends OptionLikeCheckedMatcher[SyncIO[T], Throwable](
      "a Failure",
      (_: SyncIO[T]).attempt.unsafeRunSync().left.toOption,
      check
    )
