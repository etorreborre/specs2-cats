package org.specs2.matcher

import cats.effect.IO
import org.specs2.execute.*
import org.specs2.specification.core.*
import cats.effect.unsafe.*

/** This is 
  *
  */
trait IOExecution:
  given [T : AsResult]: AsExecution[IO[T]] with
    def execute(io: =>IO[T]): Execution =
      Execution.withEnvAsync { env =>
        given IORuntime = IORuntime.global
        io.unsafeToFuture()
      }
