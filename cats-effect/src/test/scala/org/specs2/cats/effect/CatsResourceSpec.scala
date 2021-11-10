package org.specs2.cats.effect

import cats.effect.IO
import cats.effect.Ref
import org.specs2.Specification
import cats.effect.Resource

trait LocalCatsResourceSpec extends Specification, IOExecution, CatsResource[(Int, Int)]:
  import LocalCatsResourceSpec.*

  val localCounter = Ref.unsafe[IO, Int](0)
  val released = Ref.unsafe[IO, Boolean](false)

  def resource: Resource[IO, (Int, Int)] = Resource.eval(localCounter.getAndUpdate(_ + 1).product(globalCounter.getAndUpdate(_ + 1))).onFinalize(released.set(true))

  def is = s2"""
    The resource is re-acquired per-spec: ${}
  """

  def reacquired = { (i: Int) =>
    specCounter.getAndUpdate(_ + 1).map(i === _)
  }

object LocalCatsResourceSpec:
  val globalCounter = Ref.unsafe[IO, Int](0)
  val specCounter = Ref.unsafe[IO, Int](0)

trait GlobalCatsResourceSpec extends Specification, IOMatchers, CatsResource[Int]:
  import GlobalCatsResourceSpec.*

  final override def resourceKey = Some("global")

  def resource: Resource[IO, Int] = Resource.eval(counter.getAndUpdate(_ + 1)).onFinalize(released.set(true))

  def is = s2"""
    The resource has been acquired exactly once: $acquired
    The resource has not been prematurely released: $notReleased
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
