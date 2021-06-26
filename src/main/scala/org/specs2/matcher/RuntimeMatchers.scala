package org.specs2.matcher

import org.specs2.matcher.ValueChecks.valueIsTypedValueCheck
import org.specs2.matcher.describe.Diffable

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import org.specs2.text.NotNullStrings.*
import org.specs2.execute.*, Result.*

trait RunTimedMatchers[F[_]]:

  protected def runWithTimeout[A](fa: F[A], timeout: FiniteDuration): A
  protected def runAwait[A](fa: F[A]) : A

  def returnOk[T]: TimedMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, None)

  def returnValue[T](check: ValueCheck[T]): TimedMatcher[T] =
    attemptRun(check, None)

  def returnBefore[T](duration: FiniteDuration): TimedMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, Some(duration))

  protected[specs2] def attemptRun[T](check: ValueCheck[T], duration: Option[FiniteDuration]): TimedMatcher[T] =
    new TimedMatcher(check, duration)

  open class TimedMatcher[T](check: ValueCheck[T], duration: Option[FiniteDuration]) extends Matcher[F[T]]:

    def apply[S <: F[T]](e: Expectable[S]): Result =
      duration.fold(checkAwait(e.value))(checkWithDuration(e.value, _))

    def checkWithDuration(value: F[T], d: FiniteDuration): Result =
      try check.check(runWithTimeout(value, d))
      catch {
        case x: TimeoutException => timeoutResult(d)
        case x: Exception => errorResult(x)
      }

    def checkAwait(value: F[T]): Result =
      try check.check(runAwait(value))
      catch { case x: Exception => errorResult(x) }

    def before(d: FiniteDuration): TimedMatcher[T] =
      new TimedMatcher(check, Some(d))

    def withValue(check: ValueCheck[T]): TimedMatcher[T] =
      new TimedMatcher(check, duration)

    private def timeoutResult(d: FiniteDuration): Result =
      val message = s"Timeout after ${d.toMillis} milliseconds"
      result(false, message)

    private def errorResult(t: Throwable): Result =
      val message = "an exception was thrown "+t.getMessage.notNull+" "+t.getClass.getName
      result(false, message)

  // This withValue method cannot be set directly on the TimedMatcher class
  // otherwise it is always selected instead of the other withValue method
  extension [T] (timedMatcher: TimedMatcher[T])
    def withValue(t: T)(using di: Diffable[T]): TimedMatcher[T] =
      timedMatcher.withValue(valueIsTypedValueCheck(t))
