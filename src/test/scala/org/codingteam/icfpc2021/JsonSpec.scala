package org.codingteam.icfpc2021

import org.scalatest.flatspec._
import org.scalatest.matchers._

import java.math.BigInteger

class JsonSpec extends AnyFlatSpec with should.Matchers {
  "Json" should "parse an empty object" in {
    val document = """{"hole":[], "epsilon": 0, "figure": {"edges":[], "vertices":[]}}"""

    val hole = Vector[Point]()
    val figure = Figure(Vector[Edge](), Vector[Point]())
    val expectedValue = Problem(hole, 0, figure, null)

    Json.parseProblem(document) should be (expectedValue)
  }

  it should "parse a problem with large (≥2^32) coordinates" in {
    val two_to_64 = BigInt(BigInteger.ZERO).setBit(64)

    val document = """{"hole":[[-18446744073709551616, 18446744073709551616]], "epsilon": 0, "figure": {"edges":[], "vertices":[]}}"""

    val hole = Vector[Point](Point(-two_to_64, two_to_64))
    val figure = Figure(Vector[Edge](), Vector[Point]())
    val expectedValue = Problem(hole, 0, figure, null)

    Json.parseProblem(document) should be (expectedValue)
  }

  it should "parse a problem with large (≥2^32) epsilon" in {
    val two_to_64 = BigInt(BigInteger.ZERO).setBit(64)

    val document = """{"hole":[], "epsilon": 18446744073709551616, "figure": {"edges":[], "vertices":[]}}"""

    val hole = Vector[Point]()
    val figure = Figure(Vector[Edge](), Vector[Point]())
    val expectedValue = Problem(hole, two_to_64, figure, null)

    Json.parseProblem(document) should be (expectedValue)
  }

  it should "serialize bonus edge properly" in {
    val solution = Solution(Vector(Point(1, 2)), Vector(BonusUsage("BREAK_A_LEG", 123, Some(Edge(1, 2)))))
    val serialized = Json.serializeSolution(solution)
    val deserialized = Json.parseSolution(serialized)
    deserialized should be (solution)
  }
}
