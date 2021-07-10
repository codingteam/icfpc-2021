package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import java.math.BigInteger
import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.solver._

class DumbSolverSpec extends AnyFlatSpec with should.Matchers {
  "Brezenhem" should "return circle" in {
    val points = DumbSolver.brezenhem(5)

    val expected = List()

    for (point <- points) {
      println(point)
    }
    //points should be expected
  }

  "Triangle" should "be solved" in {
    val p1 = Point(0, 0)
    val p2 = Point(9, 3)
    val (r1, r2) = DumbSolver.calcThirdPoint(p1, p2, 5, 5)
    println(r1)
    println(r2)
  }
}
