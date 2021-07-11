package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import org.codingteam.icfpc2021.sat_solver._

class BooleanLogicSpec extends AnyFlatSpec with should.Matchers {
  "BooleanLogic.toCNF" should "return nothing if not given anything" in {
    val logic = new BooleanLogic()
    (logic.toCNF()) should equal (And())
  }

  it should "return the CNF it was given" in {
    val x = Term(0)
    val y = Term(1)
    val z = Term(2)

    val expression = And(Or(x, y), Not(z))

    val logic = BooleanLogic()
    logic.and(expression)
    (logic.toCNF()) should equal (expression)
  }

  it should "accumulate expressions it is given" in {
    val (x, y, z) = (Term(0), Term(1), Term(2))

    val logic = BooleanLogic()
    logic.and(And(x))
    logic.and(And(y))
    logic.and(And(z))

    val expected = And(x, y, z)

    (logic.toCNF()) should equal (expected)
  }
}
