package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._

import java.math.BigInteger

import org.codingteam.icfpc2021._

class FigureSpec extends AnyFlatSpec with should.Matchers {
  "Figure.triangles" should "return nothing for a simple segment" in {
    val vertices = Vector(Point(0, 0), Point(1, 1))
    val edges = Vector(Edge(0, 1))
    val figure = Figure(edges, vertices)

    (figure.triangles) should be (Vector())
  }

  it should "return nothing for a square" in {
    val vertices = Vector(Point(100, 100), Point(100, 200), Point(200, 100), Point(200, 200))
    val edges = Vector(Edge(0, 1), Edge(0, 2), Edge(1, 3), Edge(2, 3))
    val figure = Figure(edges, vertices)

    (figure.triangles) should be (Vector())
  }

  it should "return indices of all three points for a triangle" in {
    val vertices = Vector(Point(0, 0), Point(0, 100), Point(100, 0))
    val edges = Vector(Edge(0, 1), Edge(0, 2), Edge(1, 2))
    val figure = Figure(edges, vertices)

    (figure.triangles) should be (Vector((0, 1, 2)))
  }

  it should "return indices of all three triangles in problem 33" in {
    val problem = Json.parseProblem("""{"hole":[[0,19],[1,8],[0,0],[24,0],[26,12],[36,4],[33,17],[36,25],[36,43],[34,51],[36,60],[34,71],[27,61],[20,64],[12,71],[0,71],[0,63],[1,47],[0,37],[2,28]],"epsilon":15533,"figure":{"edges":[[0,2],[0,4],[1,2],[1,3],[2,3],[2,4],[3,4]],"vertices":[[14,34],[34,2],[0,0],[15,31],[36,3]]}}""")

    (problem.figure.triangles) should be (Vector((0, 2, 4), (1, 2, 3), (2, 3, 4)))
  }

  it should "ignore non-triangles connected to triangles" in {
    val vertices = Vector(
      // triangle
      Point(0, 10), Point(10, 0), Point(10, 10),
      // the other end of the segment starting at the triangle's vertex
      Point(0, 5),
      // other vertices of a quadrilateral which shared two vertices with the triangle
      Point(20, 5), Point(15, 20))
    val edges = Vector(
      // triangle
      Edge(0, 1), Edge(0, 2), Edge(1, 2),
      // segment
      Edge(2, 3),
      // a quadrilateral
      Edge(0, 5), Edge(1, 4), Edge(4, 5))
    val figure = Figure(edges, vertices)

    (figure.triangles) should be (Vector((0, 1, 2)))
  }

  "Figure.triangleGroups" should "return a single group for a triangle" in {
    val vertices = Vector(Point(0, 0), Point(0, 100), Point(100, 0))
    val edges = Vector(Edge(0, 1), Edge(0, 2), Edge(1, 2))
    val figure = Figure(edges, vertices)

    (figure.triangleGroups) should be (Set(Vector((0, 1, 2))))
  }

  it should "return a single group for problem 33" in {
    val problem = Json.parseProblem("""{"hole":[[0,19],[1,8],[0,0],[24,0],[26,12],[36,4],[33,17],[36,25],[36,43],[34,51],[36,60],[34,71],[27,61],[20,64],[12,71],[0,71],[0,63],[1,47],[0,37],[2,28]],"epsilon":15533,"figure":{"edges":[[0,2],[0,4],[1,2],[1,3],[2,3],[2,4],[3,4]],"vertices":[[14,34],[34,2],[0,0],[15,31],[36,3]]}}""")

    (problem.figure.triangleGroups) should be (Set(Vector((0, 2, 4), (1, 2, 3), (2, 3, 4))))
  }

  it should "return two groups for a figure that consists of two disconnected triangles" in {
    val vertices = Vector(
      // first triangle
      Point(0, 0), Point(0, 10), Point(10, 0),
      // second triangle
      Point (100, 100), Point(100, 200), Point(200, 100))
    val edges = Vector(
      // first triangle
      Edge(0, 1), Edge(0, 2), Edge(1, 2),
      // second triangle
      Edge(3, 4), Edge(3, 5), Edge(4, 5))
    val figure = Figure(edges, vertices)

    val expected = Set(
      // first triangle
      Vector((0, 1, 2)),
      // second triangle
      Vector((3, 4, 5)))

    (figure.triangleGroups) should be (expected)
  }

  it should "ignore non-triangles connected to triangles" in {
    val vertices = Vector(
      // triangle
      Point(0, 10), Point(10, 0), Point(10, 10),
      // the other end of the segment starting at the triangle's vertex
      Point(0, 5),
      // other vertices of a quadrilateral which shared two vertices with the triangle
      Point(20, 5), Point(15, 20))
    val edges = Vector(
      // triangle
      Edge(0, 1), Edge(0, 2), Edge(1, 2),
      // segment
      Edge(2, 3),
      // a quadrilateral
      Edge(0, 5), Edge(1, 4), Edge(4, 5))
    val figure = Figure(edges, vertices)

    (figure.triangleGroups) should be (Set(Vector((0, 1, 2))))
  }
}
