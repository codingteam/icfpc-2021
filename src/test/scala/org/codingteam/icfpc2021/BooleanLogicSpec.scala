package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import org.codingteam.icfpc2021.sat_solver._

class BooleanLogicSpec extends AnyFlatSpec with should.Matchers {
  "BooleanLogic.toCNF" should "return nothing if not given anything" in {
    val logic = new BooleanLogic()
    (logic.toCNF()) should equal (new Expression(Vector()))
  }
}
