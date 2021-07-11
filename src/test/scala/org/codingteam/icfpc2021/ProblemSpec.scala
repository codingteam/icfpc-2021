package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import java.math.BigInteger

import org.codingteam.icfpc2021._

class ProblemSpec extends AnyFlatSpec with should.Matchers {
  "Problem.isPointInHole" should "say whether a Point fits into the hole (in simple case)" in {
    val vertices = Vector()
    val edges = Vector()
    val figure = Figure(edges, vertices)

    val hole = Vector(Point(0, 0), Point(0, 10), Point(10, 0), Point(10, 10))
    val epsilon = 1
    val bonuses = Vector()
    val problem = Problem(hole, epsilon, figure, bonuses)

    (problem.isPointInHole(Point(0, 0))) shouldBe true
    (problem.isPointInHole(Point(1, 1))) shouldBe true
    (problem.isPointInHole(Point(100, 100))) shouldBe false
    (problem.isPointInHole(Point(1000, 500))) shouldBe false
  }

  it should "say whether or not a Point fits into a hole (in problem 3)" in {
    val problem = Json.parseProblem("""{"bonuses":[{"bonus":"GLOBALIST","problem":60,"position":[45,110]}],"hole":[[50,70],[35,75],[35,65],[15,55],[30,45],[25,30],[30,30],[30,15],[45,25],[55,35],[55,15],[65,20],[80,5],[85,25],[90,25],[80,45],[95,45],[105,50],[100,65],[85,70],[90,85],[65,80],[60,85],[55,70],[50,110],[45,110]],"epsilon":180000,"figure":{"edges":[[9,17],[17,22],[22,27],[27,19],[19,14],[14,8],[8,9],[22,28],[28,30],[9,6],[6,4],[19,23],[23,24],[24,20],[20,21],[14,10],[10,11],[11,15],[15,16],[23,29],[29,32],[10,7],[7,2],[24,33],[33,35],[11,3],[3,0],[21,25],[25,26],[26,18],[18,13],[13,12],[12,16],[15,5],[5,1],[20,31],[31,34],[16,21]],"vertices":[[15,70],[25,100],[30,35],[30,55],[35,10],[35,75],[40,25],[40,40],[45,35],[50,25],[50,50],[50,60],[50,75],[50,95],[55,45],[55,65],[55,70],[60,20],[60,105],[65,45],[65,65],[65,70],[70,25],[70,50],[70,60],[70,75],[70,95],[75,35],[80,25],[80,40],[85,10],[85,75],[90,35],[90,55],[95,100],[105,70]]}}""")

    (problem.isPointInHole(Point(40, 70))) shouldBe true
    (problem.isPointInHole(Point(50, 70))) shouldBe true
    (problem.isPointInHole(Point(49, 71))) shouldBe false
  }

  "Problem.isPointInHoleExact" should "say whether a Point fits into the hole (in simple case)" in {
    val vertices = Vector()
    val edges = Vector()
    val figure = Figure(edges, vertices)

    val hole = Vector(Point(0, 0), Point(0, 10), Point(10, 0), Point(10, 10))
    val epsilon = 1
    val bonuses = Vector()
    val problem = Problem(hole, epsilon, figure, bonuses)

    (problem.isPointInHoleExact(Point(0, 0))) shouldBe true
    (problem.isPointInHoleExact(Point(1, 1))) shouldBe true
    (problem.isPointInHoleExact(Point(100, 100))) shouldBe false
    (problem.isPointInHoleExact(Point(1000, 500))) shouldBe false
  }

  it should "say whether or not a Point fits into a hole (in problem 3)" in {
    val problem = Json.parseProblem("""{"bonuses":[{"bonus":"GLOBALIST","problem":60,"position":[45,110]}],"hole":[[50,70],[35,75],[35,65],[15,55],[30,45],[25,30],[30,30],[30,15],[45,25],[55,35],[55,15],[65,20],[80,5],[85,25],[90,25],[80,45],[95,45],[105,50],[100,65],[85,70],[90,85],[65,80],[60,85],[55,70],[50,110],[45,110]],"epsilon":180000,"figure":{"edges":[[9,17],[17,22],[22,27],[27,19],[19,14],[14,8],[8,9],[22,28],[28,30],[9,6],[6,4],[19,23],[23,24],[24,20],[20,21],[14,10],[10,11],[11,15],[15,16],[23,29],[29,32],[10,7],[7,2],[24,33],[33,35],[11,3],[3,0],[21,25],[25,26],[26,18],[18,13],[13,12],[12,16],[15,5],[5,1],[20,31],[31,34],[16,21]],"vertices":[[15,70],[25,100],[30,35],[30,55],[35,10],[35,75],[40,25],[40,40],[45,35],[50,25],[50,50],[50,60],[50,75],[50,95],[55,45],[55,65],[55,70],[60,20],[60,105],[65,45],[65,65],[65,70],[70,25],[70,50],[70,60],[70,75],[70,95],[75,35],[80,25],[80,40],[85,10],[85,75],[90,35],[90,55],[95,100],[105,70]]}}""")

    (problem.isPointInHoleExact(Point(40, 70))) shouldBe true
    (problem.isPointInHoleExact(Point(50, 70))) shouldBe true
    (problem.isPointInHoleExact(Point(49, 71))) shouldBe false
  }

  "Problem.intersectionsWithHoleBorder" should "return a set of intersection points" in {
    val initial_vertices = Vector(Point(20, 0), Point(20, 10))
    val initial_edges = Vector(Edge(0, 1))
    val initial_figure = Figure(initial_edges, initial_vertices)

    val hole = Vector(Point(0, 0), Point(0, 10), Point(10, 0))
    val epsilon = 1
    val bonuses = Vector()
    val problem = Problem(hole, epsilon, initial_figure, bonuses)

    val vertices = Vector(Point(0, 0), Point(10, 10))
    val solution = Solution(vertices, Vector[BonusUsage]())

    val expected = Set(Set(Point(0, 0)), Set(Point(5, 5)))

    problem.intersectionsWithHoleBorder(solution) should contain theSameElementsAs expected
  }

  "Problem.segmentGoesOutsideTheHole" should "return true if given segment intersects the hole border" in {
    val figure = Figure(Vector(), Vector())

    val hole = Vector(Point(0, 0), Point(0, 100), Point(50, 50), Point(100, 100), Point(100, 0))
    val epsilon = 1
    val bonuses = Vector()
    val problem = Problem(hole, epsilon, figure, bonuses)

    // One point outside the edge
    val intersecting1 = (Point(1, 1), Point(101, 50))
    val intersecting2 = (Point(101, 50), Point(0, 0))
    // Intersects in two places
    val intersecting3 = (Point(25, 55), Point(75, 55))
    // Somewhere far from the hole
    val intersecting4 = (Point(100, 200), Point(300, 400))

    // Lays within the hole, but touches one of its vertices
    val non_intersecting1 = (Point(1, 1), Point(0, 0))
    // Lays entirely within the hole
    val non_intersecting2 = (Point(49, 49), Point(47, 47))

    problem.segmentGoesOutsideTheHole(intersecting1) shouldBe true
    problem.segmentGoesOutsideTheHole(intersecting2) shouldBe true
    problem.segmentGoesOutsideTheHole(intersecting3) shouldBe true
    problem.segmentGoesOutsideTheHole(intersecting4) shouldBe true
    problem.segmentGoesOutsideTheHole(non_intersecting1) shouldBe false
    problem.segmentGoesOutsideTheHole(non_intersecting2) shouldBe false
  }
}
