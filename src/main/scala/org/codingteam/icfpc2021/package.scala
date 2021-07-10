package org.codingteam

import java.awt.Color
import java.awt.image.BufferedImage
import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode
import scala.math.sqrt
import scala.swing.Graphics2D
import scala.util.Random
import scala.util.control.Breaks._

package object icfpc2021 {

  case class Point(x: BigInt, y: BigInt) {
    override def toString: String = s"($x, $y)"

    def +(other: Point): Point = Point(this.x + other.x, this.y + other.y)

    def -(other: Point): Point = Point(this.x - other.x, this.y - other.y)

    def *(k: Double): Point = Point(BigInt((this.x.toDouble * k).toLong), BigInt((this.y.toDouble * k).toLong))

    def abs(): Double = {
      sqrt((x * x + y * y).toDouble)
    }

    def normalized(): PointD = {
      val norm = abs()
      PointD(x.toDouble / norm, y.toDouble / norm)
    }

    def toPointD(): PointD = {
      PointD(x.toDouble, y.toDouble)
    }

    def toPointBD(scale: Int = 3): PointBD = {
      val k = BigInt(10) pow scale
      PointBD(BigDecimal(x * k, scale), BigDecimal(y * k, scale))
    }

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

  case class PointBD(x: BigDecimal, y: BigDecimal) {
    override def toString: String = s"($x, $y)"

    def +(other: PointBD): PointBD = PointBD(this.x + other.x, this.y + other.y)

    def -(other: PointBD): PointBD = PointBD(this.x - other.x, this.y - other.y)

    def *(k: Double): PointBD = PointBD(this.x * k, this.y * k)

    def abs(): Double = {
      sqrt((x * x + y * y).toDouble)
    }

    def normalized(): PointBD = {
      val norm = abs()
      PointBD(x.toDouble / norm, y.toDouble / norm)
    }

    def toPointD: PointD = {
      PointD(x.toDouble, y.toDouble)
    }

    def toPoint: Point = {
      Point(x.setScale(0, RoundingMode.HALF_UP).rounded.toBigInt, y.setScale(0, RoundingMode.HALF_UP).rounded.toBigInt)
    }

    def distanceSq(other: PointBD): BigDecimal = {
      val r = this - other
      r.x * r.x + r.y * r.y
    }

    def moveTowards(target: PointBD, distanceK: Double): PointBD = {
      this + (target - this) * distanceK
    }
  }

  case class PointD(x: Double, y: Double) {
    def abs(): Double = sqrt(x * x + y * y)

    def dot(other: PointD): Double = {
      x * other.x + y * other.y
    }

    def trunc(): Point = {
      Point(BigDecimal(x).toBigInt, BigDecimal(y).toBigInt)
    }

    def round(): Point = {
      Point(BigDecimal(x).setScale(0, RoundingMode.HALF_EVEN).toBigInt, BigDecimal(y).setScale(0, RoundingMode.HALF_EVEN).toBigInt)
    }

    def *(k: Double): PointD = {
      PointD(k * x, k * y)
    }

    def /(k: Double): PointD = {
      PointD(x / k, y / k)
    }

    def +(other: PointD): PointD = {
      PointD(x + other.x, y + other.y)
    }

    def -(other: PointD): PointD = {
      PointD(x - other.x, y - other.y)
    }

    def normalize(): PointD = {
      val a = abs()
      PointD(x / a, y / a)
    }
  }

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

    /// Indices of vertices of all unique trinalges within the figure.
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

    /// Like `triangles`, but triangles are divided into disconnected groups of
    //interconnected triangles. Two triangles are considered to be connected if
    //they share at least one vertex.
    lazy val triangleGroups: Set[Vector[(Int, Int, Int)]] = {
      // Maps a set of vertices into a vector of triangles that use at least one of those vertices
      var groups: mutable.Map[Set[Int], Array[(Int, Int, Int)]] =
        mutable.Map(triangles map { p => (Set(p._1, p._2, p._3), Array(p)) }: _*)

      var changed = true
      while (changed) {
        changed = false
        breakable {
          for ((k1, v1) <- groups) {
            for ((k2, v2) <- groups) {
              if (k1 != k2 && k1.intersect(k2).nonEmpty) {
                changed = true
                val new_k = k1 ++ k2
                val new_v = v1 ++: v2
                groups -= k1
                groups -= k2
                groups += (new_k -> new_v)
                break()
              }
            }
          }
        }
      }

      groups.values.map(a => a.sorted.toVector).toSet
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

    private val ClearColor: Color = Color.BLACK
    private val FillColor: Color = Color.WHITE
    private lazy val (shiftX, shiftY, scaleX, scaleY, imgSizeX, imgSizeY) = {
      require(hole.nonEmpty, "Hole points list must be not empty")
      val MaxImageSize = 4 * 1024
      val Point(shiftX, shiftY) = holeRect.min
      val Point(sizeX, sizeY) = holeRect.size
      val (imgSizeX, scaleX) = if (sizeX > MaxImageSize)
        (MaxImageSize, MaxImageSize.toDouble / sizeX.toDouble)
      else
        (sizeX.toInt, 1.0)

      val (imgSizeY, scaleY) = if (sizeX > MaxImageSize)
        (MaxImageSize, MaxImageSize.toDouble / sizeY.toDouble)
      else
        (sizeY.toInt, 1.0)

      (shiftX, shiftY, scaleX, scaleY, imgSizeX, imgSizeY)
    }
    private lazy val holeImageArray = {
      require(hole.nonEmpty, "Hole points list must be not empty")
      val holeImage = new BufferedImage(imgSizeX, imgSizeY, BufferedImage.TYPE_INT_RGB)
      val g2 = holeImage.getGraphics.asInstanceOf[Graphics2D]
      g2.setColor(ClearColor)
      g2.fillRect(0, 0, imgSizeX, imgSizeY)
      val (holeXs, holeYs, holeLength) = splitPointsIntoCoords(hole map pointToImageCoord)
      g2.setColor(FillColor)
      g2.drawPolygon(holeXs, holeYs, holeLength)
      g2.fillPolygon(holeXs, holeYs, holeLength)
      val holeImageArray = Array.ofDim[Int](imgSizeX * imgSizeY)
      holeImage.getRGB(0, 0, imgSizeX, imgSizeY, holeImageArray, 0, imgSizeX)
      holeImageArray
    }

    private def pointToImageCoord(p: Point): Point = {
      Point(((p.x - shiftX).toDouble * scaleX).toInt, ((p.y - shiftY).toDouble * scaleY).toInt)
    }

    private def splitPointsIntoCoords(ps: Seq[Point]) = {
      (ps.view.map(_.x.toInt).toArray, ps.view.map(_.y.toInt).toArray, ps.size)
    }

    private def pointIsOutsideOfImage(v: Point) = v.x < 0 || v.y < 0 || v.x >= imgSizeX || v.y >= imgSizeY

    def isPointInHole(point: Point): Boolean = {
      val imgPoint = pointToImageCoord(point)
      val i = (imgPoint.x + imgPoint.y * imgSizeX).toInt
      !pointIsOutsideOfImage(point) && (holeImageArray(i) == FillColor.getRGB)
    }

    def randomPointInHole(): Point = {
      while (true) {
        val x = BigInt((Random.nextDouble() * holeRect.size.x.toDouble).toLong) + holeRect.min.x
        val y = BigInt((Random.nextDouble() * holeRect.size.y.toDouble).toLong) + holeRect.min.y
        val p = Point(x, y)
        if (isPointInHole(p)) {
          return p
        }
      }

      // This will never be reached, but without this line Scala complains that
      // the function returns Unit
      holeRect.min
    }
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
