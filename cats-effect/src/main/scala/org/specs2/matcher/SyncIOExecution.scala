package org.specs2.matcher

import cats.effect.SyncIO
import org.specs2.execute.AsResult
import org.specs2.specification.core.AsExecution
import org.specs2.specification.core.Execution

trait SyncIOExecution:
  given [T: AsResult]: AsExecution[SyncIO[T]] with
    def execute(sio: =>SyncIO[T]): Execution =
      Execution.result(sio.unsafeRunSync())
