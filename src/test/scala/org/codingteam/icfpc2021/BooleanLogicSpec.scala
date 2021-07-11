package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import org.codingteam.icfpc2021.sat_solver._

class BooleanLogicSpec extends AnyFlatSpec with should.Matchers {
  "BooleanLogic.toCNF" should "return nothing if not given anything" in {
    val logic = BooleanLogic()
    (logic.toCNF()) should equal (AlwaysTrue)
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
    logic.and(x)
    logic.and(y)
    logic.and(z)

    val expected = And(And(x, y), z)

    (logic.toCNF()) should equal (expected)
  }

  it should "rewrite double negations" in {
    val x = Term(0)

    val logic = BooleanLogic()
    logic.and(Not(Not(x)))

    (logic.toCNF()) should equal (x)
  }

  it should "apply distributivity law" in {
    val (x, y) = (Term(0), Term(1))

    val logic = BooleanLogic()
    logic.and(Or(And(Not(x), Not(y)), And(x, y)))

    (logic.toCNF()) should equal (And(Or(x, Not(y)), Or(y, Not(x))))
  }

  it should "rewrite simple expression to CNF" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (b, c) = (Term(0), Term(1))

    val logic = BooleanLogic()
    logic.and(Not(Or(b, c)))

    (logic.toCNF()) should equal (And(Not(b), Not(c)))
  }

  it should "rewirte slightly complex expression to CNF" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (a, b, c) = (Term(0), Term(1), Term(2))

    val logic = BooleanLogic()
    logic.and(Or(a, c))
    logic.and(Or(b, c))

    (logic.toCNF()) should equal (And(Or(a, c), Or(b, c)))
  }

  it should "rewirte slightly complex expression to CNF regardless of how it was added" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (a, b, c) = (Term(0), Term(1), Term(2))

    val logic1 = BooleanLogic()
    logic1.and(Or(a, c))
    logic1.and(Or(b, c))

    val logic2 = BooleanLogic()
    logic2.and(And(Or(a, c), Or(b, c)))

    (logic1.toCNF()) should equal (logic2.toCNF())
  }

  it should "rewrite complex expression to CNF" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (a, b, d, e) = (Term(0), Term(1), Term(2), Term(3))

    val logic = BooleanLogic()
    logic.and(And(a, Or(b, And(d, e))))

    (logic.toCNF()) should equal (And(a, And(Or(b, d), Or(b, e))))
  }

  it should "rewrite complex expression to CNF regardless of how it was added" in {
    // from https://en.wikipedia.org/wiki/Conjunctive_normal_form
    val (a, b, d, e) = (Term(0), Term(1), Term(2), Term(3))

    val logic1 = BooleanLogic()
    logic1.and(And(a, Or(b, And(d, e))))

    val logic2 = BooleanLogic()
    logic2.and(a)
    logic2.and(Or(b, And(d, e)))

    (logic1.toCNF()) should equal (logic2.toCNF())
  }
}
