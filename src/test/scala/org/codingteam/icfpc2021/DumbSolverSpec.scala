package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import java.math.BigInteger
import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.solver._

class DumbSolverSpec extends AnyFlatSpec with should.Matchers {
  "Brezenhem" should "return circle centered at (0, 0) with radius of 5" in {
    val points = DumbSolver.brezenhem(5).toSet

    val expected = Set(
      Point(0, 5), Point(0, -5), Point(5, 0), Point(-5, 0),
      Point(4, 3), Point(4, -3), Point(-4, 3), Point(-4, -3),
      Point(3, 4), Point(3, -4), Point(-3, 4), Point(-3, -4))

    points should contain theSameElementsAs expected
  }

  "Triangle" should "be solved" in {
    val p1 = Point(0, 0)
    val p2 = Point(9, 3)
    val (r1, r2) = DumbSolver.calcThirdPoint(p1, p2, 5, 5)

    Seq(r1, r2) should contain theSameElementsAs Seq(Point(5, 0), Point(4, 3))
  }
}
