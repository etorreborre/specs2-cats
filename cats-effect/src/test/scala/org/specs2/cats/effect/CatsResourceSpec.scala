package org.specs2.cats.effect

import cats.data.{Kleisli, StateT}
import cats.effect.{IO, Ref, Resource, Sync}
import org.specs2.Specification
import org.specs2.execute.Result

trait LocalCatsResourceSpec extends Specification, IOMatchers, CatsResource[Ref[IO, Int]]:

  val released = Ref.unsafe[IO, Boolean](false)

  def resource: Resource[IO, Ref[IO, Int]] = Resource
    .eval(IO.ref(0))
    .onFinalize(released.set(true))

  def is = sequential ^ s2"""
    The resource is re-acquired per-spec $reacquired
    The resource is shared in the spec $shared
    The resource is not prematurely released $notReleased
  """

  def reacquired = { (ref: Ref[IO, Int]) =>
    ref.getAndUpdate(_ + 1) must beSuccess(0)
  }

  def shared = { (ref: Ref[IO, Int]) =>
    ref.getAndUpdate(_ + 1) must beSuccess(1)
  }

  def notReleased = released.get must beSuccess(false)

class LocalCatsResource1Spec extends LocalCatsResourceSpec
class LocalCatsResource2Spec extends LocalCatsResourceSpec

trait GlobalCatsResourceSpec extends Specification, IOMatchers, CatsResource[Int]:
  import GlobalCatsResourceSpec.*

  final override def resourceKey = Some("global")

  def resource: Resource[IO, Int] = Resource.eval(counter.getAndUpdate(_ + 1)).onFinalize(released.set(true))

  def is = s2"""
    The resource has been acquired exactly once $acquired
    The resource is not prematurely released $notReleased
  """

  def acquired = { (i: Int) =>
    i === 0
  }

  def notReleased = released.get must beSuccess(false)

object GlobalCatsResourceSpec:
  val counter = Ref.unsafe[IO, Int](0)
  val released = Ref.unsafe[IO, Boolean](false)

class GlobalCatsResource1Spec extends GlobalCatsResourceSpec
class GlobalCatsResource2Spec extends GlobalCatsResourceSpec
class GlobalCatsResource3Spec extends GlobalCatsResourceSpec

class TransformersResourceSpec extends Specification, IOMatchers, CatsResource[String]:
  override def resource: Resource[IO, String] = Resource.pure("outer-param")

  def kleisliProgram: Kleisli[IO, String, Result] = for {
    weCanUse <- Kleisli.pure[IO, String, String]("We can use")
    ou <- Kleisli((outerParam: String) => IO.pure(outerParam.take(2)))
    composed <- Kleisli(outerParam => IO.pure(s"$weCanUse $outerParam res${ou}rce in IO with Kleisli"))
  } yield {
    composed must beEqualTo("We can use outer-param resource in IO with Kleisli")
  }

  def stateProgram: StateT[IO, String, Result] = for {
    ou <- StateT.get[IO, String].map(_.take(2))
    _ <- StateT.modify[IO, String]("We can use " + _)
    _ <- StateT.modify[IO, String](_ + s" res${ou}rce in IO with StateT")
    composed <- StateT.get[IO, String]
  } yield {
    composed must beEqualTo("We can use outer-param resource in IO with StateT")
  }

  def is = s2"""
    The resource execution can run Kleisli $kleisliProgram
    The resource execution can run StateT $stateProgram
  """
