package org.specs2
package matcher

import cats.effect.*
import org.specs2.concurrent.*
import scala.concurrent.duration.*

class IOTimedMatchersSpec extends Specification with IOTimedMatchers with IOExecution:
  def is = s2"""

  we can check that an IO value succeeds $matcher1
  we can check that an IO value returns a specific value $matcher2
  we can check that an IO value returns before a given time $matcher3
  we can check that an IO value returns a specific value before a given time $matcher4

  an IO value can be returned directly as a result $result1

  """

  def matcher1 = IO(1) must returnOk
  def matcher2 = IO(1) must returnValue(1)
  def matcher3 = IO { Thread.sleep(100) } must returnBefore(200.millis)
  def matcher4 = IO { Thread.sleep(100); 1 } must returnValue(1).before(200.millis)

  def result1 = IO("ok" === "ok")
