package org.specs2
package matcher

import cats.effect.*
import org.specs2.concurrent.*
import scala.concurrent.duration.*

class SyncIOMatchersSpec extends Specification with SyncIOMatchers with SyncIOExecution:
  def is = s2"""

  we can check that a SyncIO value succeeds $matcher1
  we can check that a SyncIO value returns a specific value $matcher2
  we can check that a SyncIO value raises an error $matcher3
  we can check that a SyncIO value raises an error with a specific message $matcher4

  a SyncIO value can be returned directly as a result $result1

  """

  def matcher1 = SyncIO(1) must beOk
  def matcher2 = SyncIO(1) must beOk(1)
  def matcher3 = SyncIO.raiseError(new Exception) must beKo
  def matcher4 = SyncIO.raiseError(new Exception("a very special message")) must beKo.withThrowable[Exception](
    "a very special message"
  )

  def result1 = SyncIO("ok" === "ok")
