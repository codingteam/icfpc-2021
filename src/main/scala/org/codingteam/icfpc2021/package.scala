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

  object Quadrant extends Enumeration {
    type Quadrant = Value

    val First, Second, Third, Fourth = Value
  }

  /// Next quadrant, counter-clockwise
  def nextQuadrant(quad: Quadrant.Quadrant): Quadrant.Quadrant = {
    import Quadrant._
    quad match {
      case First => Second;
      case Second => Third;
      case Third => Fourth;
      case Fourth => First;
    }
  }

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

    // ported from temp/gui.py getquad()
    def quadrant(): Quadrant.Quadrant = {
      import Quadrant._

      if (x > 0 && y >= 0) {
        First
      } else if (x <= 0 && y > 0) {
        Second
      } else if (x < 0 && y <= 0) {
        Third
      } else /* if (x >= 0 && y < 0) */ {
        Fourth
      }
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

  object PointBD {
    val Zero: PointBD = PointBD(0, 0)
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
    private lazy val (holeImageArray, borderImageArray) = {
      require(hole.nonEmpty, "Hole points list must be not empty")

      val (holeXs, holeYs, holeLength) = splitPointsIntoCoords(hole map pointToImageCoord)

      val borderImage = new BufferedImage(imgSizeX, imgSizeY, BufferedImage.TYPE_INT_RGB)
      val borderG2 = borderImage.getGraphics.asInstanceOf[Graphics2D]
      borderG2.setColor(ClearColor)
      borderG2.fillRect(0, 0, imgSizeX, imgSizeY)
      borderG2.setColor(FillColor)
      borderG2.drawPolygon(holeXs, holeYs, holeLength)
      val borderImageArray = Array.ofDim[Int](imgSizeX * imgSizeY)
      borderImage.getRGB(0, 0, imgSizeX, imgSizeY, borderImageArray, 0, imgSizeX)

      val holeImage = new BufferedImage(imgSizeX, imgSizeY, BufferedImage.TYPE_INT_RGB)
      val holeG2 = holeImage.getGraphics.asInstanceOf[Graphics2D]
      holeG2.setColor(ClearColor)
      holeG2.fillRect(0, 0, imgSizeX, imgSizeY)
      holeG2.setColor(FillColor)
      holeG2.fillPolygon(holeXs, holeYs, holeLength)
      val holeImageArray = Array.ofDim[Int](imgSizeX * imgSizeY)
      holeImage.getRGB(0, 0, imgSizeX, imgSizeY, holeImageArray, 0, imgSizeX)

      (holeImageArray, borderImageArray)
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
      !pointIsOutsideOfImage(point) && (holeImageArray(i) != ClearColor.getRGB || borderImageArray(i) != ClearColor.getRGB)
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

    /** Distance range squared units.
     *
     * Given an squared distance `distSq`, return allowed square distange range
     * multiplied by 1000000.
     */
    def distRangeSqUnits(distSq: BigInt): (BigInt, BigInt) =
      (distSq * (1000000 - epsilon),
        distSq * (1000000 + epsilon))

    def edgeDistRangeSqUnits(i: Int, j: Int): (BigInt, BigInt) =
      distRangeSqUnits(figure.vertices(i).distanceSq(figure.vertices(j)))

    private def drawSolutionToFigure(solution: Solution): Array[Int] = {
      require(hole.nonEmpty, "Hole points list must be not empty")
      val figureImage = new BufferedImage(imgSizeX, imgSizeY, BufferedImage.TYPE_INT_RGB)
      val figureImageArray = Array.ofDim[Int](imgSizeX * imgSizeY)

      val g2 = figureImage.getGraphics.asInstanceOf[Graphics2D]
      g2.setColor(ClearColor)
      g2.fillRect(0, 0, imgSizeX, imgSizeY)
      g2.setColor(FillColor)
      for (e <- figure.edges) {
        val v1 = pointToImageCoord(solution.vertices(e.vertex1))
        val v2 = pointToImageCoord(solution.vertices(e.vertex2))
        g2.drawLine(v1.x.toInt, v1.y.toInt, v2.x.toInt, v2.y.toInt)
      }
      figureImage.getRGB(0, 0, imgSizeX, imgSizeY, figureImageArray, 0, imgSizeX)

      figureImageArray
    }

    /// All the points at which the figure intersects with the hole's border.
    ///
    /// Because we're using a raster, where each pixel can represent multiple
    //points of the original plane, we're returning a set of points one of
    //which is the point of intersection.
    def intersectionsWithHoleBorder(solution: Solution): Set[Set[Point]] = {
      val figureImageArray = drawSolutionToFigure(solution)

      val clearColorRGB = ClearColor.getRGB
      val fillColorRGB = FillColor.getRGB
      (for (i <- figureImageArray.indices) yield {
        if (figureImageArray(i) != clearColorRGB && borderImageArray(i) == fillColorRGB) {
          val imgY = i / imgSizeX
          val imgX = i - imgY * imgSizeX

          val expansionX = (1 / scaleX).ceil.toInt - 1
          val expansionY = (1 / scaleY).ceil.toInt - 1
          val result = (-expansionX to expansionX).flatMap(dx =>
            (-expansionY to expansionY).map(dy =>
              Point(
                BigInt((imgX.toDouble / scaleX).toLong) + shiftX + dx,
                BigInt((imgY.toDouble / scaleY).toLong) + shiftY + dy))).toSet
          result
        } else {
          Set[Point]()
        }
      }).filter(!_.isEmpty).toSet
    }


    // ported from temp/gui.py point_in_hole()
    def isPointInHoleExact(point: Point): Boolean = {
      val Point(px, py) = point

      var last_hole_point = hole.last
      var angle: Int = 0
      for (cur_hole_point <- hole) {
        val Point(x1, y1) = last_hole_point
        val Point(x2, y2) = cur_hole_point

        val q1 = (last_hole_point - point).quadrant()
        val q2 = (cur_hole_point - point).quadrant()
        if (q1 == q2) {
          // nothing to do
        } else if (nextQuadrant(q1) == q2) {
          angle += 1
        } else if (nextQuadrant(q2) == q1) {
          angle -= 1
        } else {
          val mult = (px-x1)*(y2-y1)-(x2-x1)*(py-y1)
          if (mult == 0) {
            return true // on the line
          } else if (mult < 0) {
            angle += 2
          } else { // mult > 0
            angle -= 2
          }
        }
        last_hole_point = cur_hole_point
      }

      return angle != 0
    }

    // ported from temp/gui.py line_in_hole
    // note that we invert the result (Python checks if the line is *in* the
    // hole, we're checking if it's *outside* of the hole)
    def segmentGoesOutsideTheHole(segment: (Point, Point)): Boolean = {
      val (p1, p2) = segment
      if (!isPointInHoleExact(p1) || !isPointInHoleExact(p2)) {
        return true
      }
      val Point(ax1, ay1) = p1
      val Point(ax2, ay2) = p2

      var last_hole_point = hole.last
      for (cur_hole_point <- hole) {
        val Point(bx1, by1) = hole.last
        val Point(bx2, by2) = cur_hole_point

		if (( (ax2-ax1)*(by1-ay1)-(bx1-ax1)*(ay2-ay1) ) * ( (ax2-ax1)*(by2-ay1)-(bx2-ax1)*(ay2-ay1) ) < 0
            &&
		   ( (bx2-bx1)*(ay1-by1)-(ax1-bx1)*(by2-by1) ) * ( (bx2-bx1)*(ay2-by1)-(ax2-bx1)*(by2-by1) ) < 0)
           {
			return true
           }
		last_hole_point = cur_hole_point
      }

      false
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
