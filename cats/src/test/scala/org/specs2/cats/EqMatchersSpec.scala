package org.specs2.cats

import cats.kernel.Eq
import cats.implicits.*
import org.specs2.*

class EqMatchersSpec extends Specification with EqDiffable:
  def is = s2"""

  we can use a specific cats.Eq instance with an equality expectation
    where the equality is verified $eq1
    where the equality is not verified $eq2

  """

  def eq1 =
    Person("alice", 30) === Person("alice", 31)

  def eq2 =
    (Person("alice", 30) === Person("bob", 31)).message === "Person(alice,30) != Person(bob,31)"

  case class Person(name: String, age: Int)

  given Eq[Person] =
    new Eq:
      def eqv(p1: Person, p2: Person): Boolean =
        p1.name == p2.name
