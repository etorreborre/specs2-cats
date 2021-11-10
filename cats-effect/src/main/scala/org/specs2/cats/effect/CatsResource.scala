package org.specs2.cats.effect

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Resource
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import org.specs2.specification.core.Execution
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
