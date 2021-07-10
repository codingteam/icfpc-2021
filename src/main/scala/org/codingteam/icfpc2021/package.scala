package org.codingteam

import scala.collection.mutable

package object icfpc2021 {

  case class Point(x: BigInt, y: BigInt) {
    override def toString: String = s"($x, $y)"

    def +(other: Point): Point = Point(this.x + other.x, this.y + other.y)

    def -(other: Point): Point = Point(this.x - other.x, this.y - other.y)

    def *(k: Double): Point = Point(BigInt((this.x.toDouble * k).toLong), BigInt((this.y.toDouble * k).toLong))

    def distanceSq(other: Point): BigInt = {
      val r = this - other
      r.x * r.x + r.y * r.y
    }

    def moveTowards(target: Point, distanceK: Double): Point = {
      this + (target - this) * distanceK
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

  case class Figure(edges: Vector[Edge], vertices: Vector[Point]) {
    lazy val vertexNeighbours: Int => Seq[Int] = {
      val neighbours = Array.fill(vertices.size)(mutable.Set[Int]())
      for (Edge(v1, v2) <- edges) {
        neighbours(v1) += v2
        neighbours(v2) += v1
      }
      neighbours map (_.toArray.toSeq)
    }

    lazy val triangles: Vector[(Int, Int, Int)] = {
      vertices.indices
        .flatMap(idx =>
          vertexNeighbours(idx)
            .filter(_ > idx)
            .flatMap(snd_idx =>
              vertexNeighbours(idx)
                .intersect(vertexNeighbours(snd_idx))
                .filter(_ > snd_idx)
                .map((idx, snd_idx, _)),
            ),
        )
        .toVector
    }
  }

  case class BonusSpec(bonus: String, problem: Int, position: Point)

  case class Problem(hole: Vector[Point], epsilon: BigInt, figure: Figure, bonuses: Vector[BonusSpec]) {
    lazy val holeRect: Rect = Rect(
      Point(hole.minBy(_.x).x, hole.minBy(_.y).y),
      Point(hole.maxBy(_.x).x, hole.maxBy(_.y).y))

    lazy val holeCenter: PointD = {
      val xSum = hole.view.map(_.x).sum
      val ySum = hole.view.map(_.y).sum
      PointD(xSum.toDouble / hole.size, ySum.toDouble / hole.size)
    }
    val figureVerticesCount: Int = figure.vertices.size
  }

  case class BonusUsage(bonus: String, problem: Int, edge: Option[Edge])
  case class Solution(vertices: Vector[Point], bonuses: Vector[BonusUsage])

  /**
   * Границы 2D
   *
   * @param min минимальная точка (включительно).
   * @param max максимальная точка (включительно).
   */
  case class Rect(min: Point, max: Point) {
    lazy val size: Point = max - min + Point.Ones

    def contains(p: Point): Boolean =
      p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y
  }
}
