package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import org.codingteam.icfpc2021.sat_solver._

class DIMACSSpec extends AnyFlatSpec with should.Matchers {
  "DIMACS.from" should "return empty task if the logic is always true" in {
    val logic = BooleanLogic()

    (DIMACS.from(logic)) should equal ("")
  }

  it should "return empty task if the logic is always false" in {
    val logic = BooleanLogic()
    logic.and(AlwaysFalse)

    (DIMACS.from(logic)) should equal ("")
  }

  it should "return a single clause if logic consists of a single OR statement" in {
    val (a, b) = (Term(1), Term(2))
    val logic = BooleanLogic()
    logic.and(Or(a, Not(b)))

    val expected =
      """p cnf 2 1
        |1 -2 0""".stripMargin

    (DIMACS.from(logic)) should equal (expected)
  }

  it should "return multiple clauses (1)" in {
    val (x, y, z) = (Term(1), Term(2), Term(3))
    val expression = And(Or(x, y), Not(z))
    val logic = BooleanLogic()
    logic.and(expression)

    val expected =
      """p cnf 3 2
        |-3 0
        |1 2 0""".stripMargin


    (DIMACS.from(logic)) should equal (expected)
  }

  it should "return multiple clauses (2)" in {
    val logic = BooleanLogic()
    logic.and(Term(1))
    logic.and(Term(2))
    logic.and(Term(3))

    val expected =
      """p cnf 3 3
        |3 0
        |2 0
        |1 0""".stripMargin

    (DIMACS.from(logic)) should equal (expected)
  }

  it should "return multiple clauses (3)" in {
    val (x, y) = (Term(1), Term(2))
    val logic = BooleanLogic()
    logic.and(And(Or(x, Not(y)), Or(y, Not(x))))

    val expected =
      """p cnf 2 2
        |2 -1 0
        |1 -2 0""".stripMargin

    (DIMACS.from(logic)) should equal (expected)
  }

  it should "return multiple clauses (4)" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (b, c) = (Term(1), Term(2))
    val logic = BooleanLogic()
    logic.and(And(Not(b), Not(c)))

    val expected =
      """p cnf 2 2
        |-2 0
        |-1 0""".stripMargin

    (DIMACS.from(logic)) should equal (expected)
  }

  it should "return multiple clauses (5)" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (a, b, c) = (Term(1), Term(2), Term(3))
    val logic = BooleanLogic()
    logic.and(And(Or(a, c), Or(b, c)))

    val expected =
      """p cnf 3 2
        |2 3 0
        |1 3 0""".stripMargin

    (DIMACS.from(logic)) should equal (expected)
  }

  it should "return multiple clauses (6)" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (a, b, d, e) = (Term(1), Term(2), Term(3), Term(4))
    val logic = BooleanLogic()
    logic.and(And(a, And(Or(b, d), Or(b, e))))

    val expected =
      """p cnf 4 3
        |2 4 0
        |2 3 0
        |1 0""".stripMargin

    (DIMACS.from(logic)) should equal (expected)
  }
}
