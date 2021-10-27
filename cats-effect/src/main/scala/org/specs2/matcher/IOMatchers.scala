package org.specs2
package matcher

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import org.specs2.concurrent.ExecutionEnv
import org.specs2.execute.Failure
import org.specs2.execute.Result
import org.specs2.execute.Success
import org.specs2.text.Regexes.matchesSafely

import scala.concurrent.Future

/** Matchers for `IO`. These will run asynchronously on the (overridable) `IORuntime`.
  */
trait IOMatchers extends ValueChecks:

  given executionEnv: ExecutionEnv =
    ExecutionEnv.fromGlobalExecutionContext

  given ioRuntime: IORuntime = IORuntime.global

  extension [A](ioa: IO[A])
    private def unsafeFoldOutcome[B](canceled: =>B, errored: Throwable => B, completed: A => B): Future[B] =
      ioa.start
        .flatMap(_.join)
        .flatMap { outcome =>
          outcome.fold(IO(canceled), e => IO(errored(e)), _.map(completed))
        }
        .unsafeToFuture()

  def beSuccess[A]: FutureMatcher[IO[A]] =
    beSuccess(ValueCheck.alwaysOk)

  def beSuccess[A](check: ValueCheck[A]): FutureMatcher[IO[A]] =
    FutureMatcher(
      _.unsafeFoldOutcome(
        Failure("The IO was canceled"),
        Error("The IO raised an error", _),
        check.check
      )
    )

  def beError[A]: FutureMatcher[IO[A]] =
    beError(ValueCheck.alwaysOk)

  def beError[A](check: ValueCheck[Throwable]): FutureMatcher[IO[A]] =
    FutureMatcher(
      _.>>(IO.trace).unsafeFoldOutcome(
        Failure("The IO was canceled"),
        check.check,
        trace => Failure("The IO succeeded but it was expected to raise an error", trace = trace.toList)
      )
    )

  def beCanceled[A]: FutureMatcher[IO[A]] =
    FutureMatcher(
      _.>>(IO.trace).unsafeFoldOutcome(
        Success("The IO was canceled"),
        Error("The IO raised an error", _),
        trace => Failure("The IO succeeded but it was expected to cancel", trace = trace.toList)
      )
    )

  extension [T](action: IO[T])
    infix def must(m: FutureMatcher[IO[T]]): Future[Result] =
      m(action)
