package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import java.math.BigInteger
import math.sqrt
import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.solver._

class DumbSolverSpec extends AnyFlatSpec with should.Matchers {
  "Brezenhem" should "return a circle with radius of 5" in {
    val points = DumbSolver.brezenhem(1e-300, 5).toSet

    val expected = Set(
      Point(0, 5), Point(0, -5), Point(5, 0), Point(-5, 0),
      Point(4, 3), Point(4, -3), Point(-4, 3), Point(-4, -3),
      Point(3, 4), Point(3, -4), Point(-3, 4), Point(-3, -4))

    points should contain theSameElementsAs expected
  }

  it should "return circle with non-integer radius" in {
    val radius = sqrt(99*99 + 1*1)
    val epsilon = (radius - 99) / 2

    val points = DumbSolver.brezenhem(epsilon, radius).toSet

    points should contain (Point(71, 69))
    points should contain (Point(91, 39))
    points should contain (Point(99, 1))
    // Each of the three points above has 8 variations: 4 variations of sights,
    // and 2 variations where X and Y are swapped
    points should have size 24
  }

  "Triangle" should "be solved" in {
    val p1 = Point(0, 0)
    val p2 = Point(9, 3)
    val (r1, r2) = DumbSolver.calcThirdPoint(p1, p2, 5, 5)

    Seq(r1, r2) should contain theSameElementsAs Seq(Point(5, 0), Point(4, 3))
  }

  "Mirror" should "work" in {
    val p1 = Point(0, 0)
    val p2 = Point(3, 3)
    val p = Point(1, 0)

    val r = DumbSolver.mirror(p, p1, p2)
    r should be (Point(0, 1))
  }
}
