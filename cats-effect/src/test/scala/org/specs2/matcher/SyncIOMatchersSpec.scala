package org.specs2
package matcher

import cats.effect.*
import org.specs2.concurrent.*
import scala.concurrent.duration.*

class SyncIOMatchersSpec extends Specification with SyncIOMatchers with SyncIOExecution:
  def is = s2"""

  we can check that a SyncIO succeeds $matcher1
  we can check that a SyncIO succeeds with a specific value $matcher2
  we can check that a SyncIO raises an error $matcher3
  we can check that a SyncIO raises a specific error $matcher4

  a SyncIO value can be returned directly as a result $result1

  """

  class MyException(msg: String) extends Exception(msg)

  def matcher1 = SyncIO(1) must beSuccess
  def matcher2 = SyncIO(1) must beSuccess(1)
  def matcher3 = SyncIO.raiseError(new Exception) must beError
  def matcher4 = SyncIO.raiseError(new MyException("my message")) must beError(new MyException("my message"))

  def result1 = SyncIO("ok" === "ok")
