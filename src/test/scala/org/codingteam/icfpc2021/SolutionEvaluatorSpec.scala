package org.codingteam.icfpc2021

import org.codingteam.icfpc2021.evaluator._
import org.scalatest.flatspec._
import org.scalatest.matchers._

import java.math.BigInteger

class SolutionEvaluatorSpec extends AnyFlatSpec with should.Matchers {
  "SolutionEvaluator" should "return 0 for solution that matches the problem" in {
    val hole = Vector(Point(0, 0), Point(10, 0), Point(10, 10), Point(0, 10))
    val figure = Figure(Vector(Edge(0, 1), Edge(1, 2), Edge(2, 3), Edge(3, 0)), hole)
    val problem = Problem(hole, 0, figure, null)

    val solution = Solution(hole, null)

    val evaluator = new SolutionEvaluator(problem)

    evaluator.evaluate(solution) should be (new BigInt(BigInteger.ZERO))
  }

  it should "return non-zero if solution doesn't exactly match the hole of the problem" in {
    val hole = Vector(Point(0, 0), Point(10, 0), Point(10, 10), Point(0, 10))
    val figure = Figure(Vector(Edge(0, 1), Edge(1, 2), Edge(2, 3), Edge(3, 0)), hole)
    val problem = Problem(hole, 0, figure, null)

    // All points are moved by 1 pixel "inwards" of the hole
    val solution = Solution(Vector(Point(1, 1), Point(9, 1), Point(9, 9), Point(1, 9)), null)

    val evaluator = new SolutionEvaluator(problem)

    evaluator.evaluate(solution) should be > new BigInt(BigInteger.ZERO)
  }
}
