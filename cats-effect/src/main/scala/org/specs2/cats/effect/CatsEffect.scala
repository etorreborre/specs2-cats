package org.specs2.cats.effect

import cats.effect.unsafe.IORuntime
import org.specs2.concurrent.ExecutionEnv

trait CatsEffect(using
    executionEnv: ExecutionEnv = ExecutionEnv.fromGlobalExecutionContext,
    ioRuntime: IORuntime = IORuntime.global
) extends IOExecution,
      IOMatchers,
      SyncIOExecution,
      SyncIOMatchers,
      CatsResourceExecution
