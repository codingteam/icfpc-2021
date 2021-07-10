package org.codingteam

package object icfpc2021 {

  case class Point(x: BigInt, y: BigInt) {
    override def toString: String = s"($x, $y)"

    def +(other: Point): Point = Point(this.x + other.x, this.y + other.y)

    def -(other: Point): Point = Point(this.x - other.x, this.y - other.y)

    def distanceSq(other: Point): BigInt = {
      val r = this - other
      r.x * r.x + r.y * r.y
    }
  }

  object Point {
    val Zero: Point = Point(0, 0)
    val Ones: Point = Point(1, 1)
  }

  case class PointD(x: Double, y: Double)

  case class Edge(vertex1: Int, vertex2: Int) {
    override def toString: String = s"v$vertex1 - v$vertex2"
  }

  case class Polygon(vertexes: Vector[Int])

  case class Figure(edges: Vector[Edge], vertices: Vector[Point])

  case class Problem(hole: Vector[Point], epsilon: BigInt, figure: Figure) {
    lazy val holeRect: Rect = Rect(
      Point(hole.minBy(_.x).x, hole.minBy(_.y).y),
      Point(hole.maxBy(_.x).x, hole.maxBy(_.y).y),
    )

    lazy val holeCenter: PointD = {
      val xSum = hole.view.map(_.x).sum
      val ySum = hole.view.map(_.y).sum
      PointD(xSum.toDouble / hole.size, ySum.toDouble / hole.size)
    }
    val figureVerticesCount: Int = figure.vertices.size
  }

  case class Solution(vertices: Vector[Point])

  /** Границы 2D
    *
    * @param min
    *   минимальная точка (включительно).
    * @param max
    *   максимальная точка (включительно).
    */
  case class Rect(min: Point, max: Point) {
    lazy val size: Point = max - min + Point.Ones

    def contains(p: Point): Boolean =
      p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y
  }
}
