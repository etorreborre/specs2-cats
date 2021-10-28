package org.specs2
package matcher

import cats.effect.SyncIO
import execute.ResultImplicits.*
import org.specs2.execute.Result
import org.specs2.execute.Error
import org.specs2.execute.Failure

import scala.reflect.ClassTag

trait SyncIOMatchers extends ValueChecks:

  extension [A](sioa: SyncIO[A])
    private def unsafeFoldOutcome[B](errored: Throwable => B, completed: A => B): B =
      sioa.attempt
        .map {
          case Left(ex) => errored(ex)
          case Right(a) => completed(a)
        }
        .unsafeRunSync()

  def beSuccess[A]: Matcher[SyncIO[A]] =
    beSuccess(ValueCheck.alwaysOk)

  def beSuccess[A](check: ValueCheck[A]): Matcher[SyncIO[A]] =
    Matcher[SyncIO[A], Result](
      _.unsafeFoldOutcome(
        Error("The SyncIO raised an error", _),
        check.check
      )
    )

  def beError[A]: Matcher[SyncIO[A]] =
    beError(ValueCheck.alwaysOk)

  def beError[A](check: ValueCheck[Throwable]): Matcher[SyncIO[A]] =
    Matcher[SyncIO[A], Result](
      _.unsafeFoldOutcome(
        check.check,
        a => Failure("The SyncIO succeeded but it was expected to raise an error")
      )
    )
