package org.specs2.cats.effect

import cats.FlatMap
import cats.data.{Kleisli, StateT}
import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Resource
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import org.specs2.specification.core.{AsExecution, Execution}
import org.specs2.specification.Resource as SpecsResource

import scala.concurrent.Future

trait CatsResource[T](using ioRuntime: IORuntime = IORuntime.global) extends SpecsResource[T], IOExecution:
  private val finalizer = Deferred.unsafe[IO, IO[Unit]]

  def resource: Resource[IO, T]

  protected def acquire: Future[T] = resource.attempt.allocated
    .flatMap { (t, close) =>
      finalizer.complete(close).as(t)
    }
    .rethrow
    .unsafeToFuture()(ioRuntime)

  protected def release(resource: T): Execution = finalizer.get.flatten

  /** When the execution is based on some input parameter prepared by resource */
  given kleisliExecution[F[_], R](using ae: AsExecution[T => F[R]]): AsExecution[Kleisli[F, T, R]] =
    new AsExecution[Kleisli[F, T, R]] {
      override def execute(t: =>Kleisli[F, T, R]): Execution = ae.execute(t.run)
    }

  /** When the stateful execution requires initial state prepared by resource */
  given stateTexecution[F[_]: FlatMap, R](using ae: AsExecution[T => F[R]]): AsExecution[StateT[F, T, R]] =
    new AsExecution[StateT[F, T, R]] {
      override def execute(t: =>StateT[F, T, R]): Execution = ae.execute(resourceState => t.runA(resourceState))
    }
