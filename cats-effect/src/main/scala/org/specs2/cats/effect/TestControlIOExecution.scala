package org.specs2.cats.effect

import cats.effect.IO
import cats.effect.testkit.TestControl
import org.specs2.execute.AsResult
import org.specs2.specification.core.{AsExecution, Execution}

trait TestControlIOExecution extends IOExecution:
  given testControlExecution[T: AsResult]: AsExecution[IO[T]] with
    def execute(io: =>IO[T]): Execution =
      given_AsExecution_IO[T].execute(TestControl.executeEmbed(io))
