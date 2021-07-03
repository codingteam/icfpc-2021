package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

class ExampleSpec extends AnyFlatSpec with should.Matchers {
  "2+2" should "be equal to 4" in {
    (2+2) should be (4)
  }
}
