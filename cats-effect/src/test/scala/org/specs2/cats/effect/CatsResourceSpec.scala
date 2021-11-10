package org.specs2.cats.effect

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import org.specs2.Specification
import org.specs2.execute.Result

trait LocalCatsResourceSpec extends Specification, IOExecution, IOMatchers, CatsResource[(Int, Int)]:
  import LocalCatsResourceSpec.*

  val localCounter = Ref.unsafe[IO, Int](0)
  val released = Ref.unsafe[IO, Boolean](false)

  def resource: Resource[IO, (Int, Int)] = Resource
    .eval(localCounter.getAndUpdate(_ + 1).product(globalCounter.getAndUpdate(_ + 1)))
    .onFinalize(released.set(true))

  def is = s2"""
    The resource is re-acquired per-spec $reacquired
    The resource is shared in the spec $shared
    The resource is shared in the spec $shared
    The resource is not prematurely released $notReleased
  """

  def reacquired: ((Int, Int)) => IO[Result] = { (_, globalCount) =>
    specCounter.getAndUpdate(_ + 1).map(globalCount === _)
  }

  def shared: ((Int, Int)) => Result = { (resource, _) =>
    resource === 0
  }

  def notReleased = released.get must beSuccess(false)

object LocalCatsResourceSpec:
  val globalCounter = Ref.unsafe[IO, Int](0)
  val specCounter = Ref.unsafe[IO, Int](0)

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
