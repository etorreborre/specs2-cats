package org.specs2.matcher

import cats.effect.*
import org.specs2.execute.*
import org.specs2.specification.core.*
import cats.effect.unsafe.*
import scala.concurrent.duration.*

/** This is
  *
  */
trait IOExecution:
  given [T : AsResult]: AsExecution[IO[T]] with
    def execute(io: =>IO[T]): Execution =
      Execution.withEnvAsync { env =>
        given IORuntime = makeIORuntime(env)
        io.unsafeToFuture()
      }

  /** override this method to provide your own IORuntime */
  protected def makeIORuntime(env: Env): IORuntime =
    IORuntime(
      compute = env.executionContext,
      blocking = env.executionContext,
      scheduler = makeScheduler(env),
      shutdown = () => (),
      config = IORuntimeConfig())

  private def makeScheduler(env: Env): Scheduler =
    new Scheduler:
      def sleep(delay: FiniteDuration, task: Runnable): Runnable =
        val cancel = env.executionEnv.scheduler.schedule(task.run(), delay)
        new Runnable:
          def run: Unit = cancel()

      def nowMillis() = System.currentTimeMillis()
      def monotonicNanos() = System.nanoTime()
