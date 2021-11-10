package org.specs2.cats.effect

import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all.*
import org.specs2.execute.AsResult
import org.specs2.specification.core.AsExecution
import org.specs2.specification.core.Execution

trait CatsResourceExecution:
  given [F[_]: MonadCancelThrow, T](using exec: AsExecution[F[T]]): AsExecution[Resource[F, T]] with
    def execute(r: =>Resource[F, T]): Execution =
      exec.execute(r.use(_.pure))
