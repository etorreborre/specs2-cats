package org.specs2
package matcher

import cats.effect.*
import org.specs2.concurrent.*
import scala.concurrent.duration.*

class IOMatchersSpec extends Specification with IOMatchers with IOExecution:
  def is = s2"""

  we can check that an IO value succeeds $matcher1
  we can check that an IO value returns a specific value $matcher2
  we can check that an IO value raises an error $matcher3
  we can check that an IO value raises an error with a specific message $matcher4

  an IO value can be returned directly as a result $result1

  """

  def matcher1 = IO(1) must beOk
  def matcher2 = IO(1) must beOkWithValue(1)
  def matcher3 = IO.raiseError(new Exception) must beKo
  def matcher4 = IO.raiseError(new Exception("a very special message")) must beKo("a very special message")

  def result1 = IO("ok" === "ok")
