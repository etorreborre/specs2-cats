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

  def beOk[T]: FutureMatcher[IO[T]] =
    FutureMatcher((_: IO[T]).unsafeToFuture())

  def beOk[T](check: ValueCheck[T]): FutureMatcher[IO[T]] =
    FutureMatcher((_: IO[T]).map(check.check).unsafeToFuture())

  def beKo[T]: FutureMatcher[IO[T]] =
    FutureMatcher { (action: IO[T]) =>
      action.map(_ => Failure("a failure was expected")).recover(_ => Success("ok")).unsafeToFuture()
    }

  def beKo[T](message: String): FutureMatcher[IO[T]] =
    FutureMatcher { (action: IO[T]) =>
      action
        .map(_ => Failure(s"a failure with message $message was expected"))
        .recover {
          case t if t.getMessage `matchesSafely` message => Success("ok")
          case t => Failure(s"the action failed with message ${t.getMessage}. Expected: $message"),
        }
        .unsafeToFuture()
    }

  extension [T](action: IO[T])
    infix def must(m: FutureMatcher[IO[T]]): Future[Result] =
      m(action)
