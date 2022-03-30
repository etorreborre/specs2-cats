package org.specs2.cats.effect

import cats.effect.*
import org.specs2.Specification
import org.specs2.concurrent.*
import org.specs2.matcher.ValueChecks
import org.specs2.specification.core.Execution

import scala.concurrent.duration.*

class IOMatchersSpec extends Specification with IOMatchers with IOExecution:
  def is = s2"""

  we can check that an IO succeeds $matcher1
  we can check that an IO succeeds with a specific value $matcher2
  we can check that an IO raises an error $matcher3
  we can check that an IO raises a specific error $matcher4
  we can check that an IO cancels $matcher5

  an IO value can be returned directly as a result $result1

  """

  class MyException(msg: String) extends Exception(msg)

  def matcher1 = IO(1) must beSuccess
  def matcher2 = IO(1) must beSuccess(1)
  def matcher3 = IO.raiseError(new Exception) must beError
  def matcher4 = IO.raiseError(new MyException("my message")) must beError(beException[MyException]("my message"))
  def matcher5 = IO.canceled must beCanceled

  def result1 = IO("ok" === "ok")

class IOTestControlSpec extends Specification with ValueChecks with TestControlIOExecution:

  def fakeSleepExecution = (IO.sleep(10.days).as(1 === 1): Execution).setTimeout(10.seconds)

  def is = s2"""
    An IO test runs in mock time with TestControlIOExecution
    $fakeSleepExecution
  """
