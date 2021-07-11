package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Figure, Point, Problem, Solution}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode
import scala.math.{abs, sqrt}

object DumbSolver {
  def brezenhem(epsilon: Double, radius: Double) : Seq[Point] = {
    val result = new mutable.ListBuffer[Point]()
    val center = Point(0,0)
    val r2 = radius*radius
    var x = 0
    var y = radius.round
    var delta = 1 - 2*radius
    var error: Double = 0
    while (y >= x) {
      val p = Point(x, y)
      if (abs(sqrt((p distanceSq center).toDouble / r2) - 1) <= epsilon/1e6) {
        result += p
        result += Point(x, -y)
        result += Point(-x, y)
        result += Point(-x, -y)
        result += Point(y, x)
        result += Point(y, -x)
        result += Point(-y, x)
        result += Point(-y, -x)
      }
      error = 2 * (delta+y) - 1
      if ((delta < 0) && (error <= 0)) {
        x += 1
        delta += 2 * x + 1
      } else if ((delta > 0) && (error > 0)) {
        y -= 1
        delta -= 2 * y + 1
      } else {
        x += 1
        y -= 1
        delta += 2*(x - y)
      }
    }
    result.toSeq
  }

  def calcThirdPoint(p1: Point, p2: Point, r1: BigInt, r2: BigInt) : (Point, Point) = {
    val (x1, y1) = (p1.x.toDouble, p1.y.toDouble)
    val (x2, y2) = (p2.x.toDouble, p2.y.toDouble)
    val (r1d, r2d) = (r1.toDouble, r2.toDouble)
    val (r12, r22) = (r1d*r1d, r2d*r2d)
    val (r14, r24) = (r12*r12, r22*r22)
    val d2 = (p1 distanceSq p2).toDouble
    val d = sqrt(d2)
    val (dirX, dirY) = ((x2-x1)/d, (y2-y1)/d)

    val cosAlpha = (r12 - r22 + d2) / (2*r1d*d)
    val cosBeta = (r22 - r12 + d2) / (2*r2d*d)

    val sinAlpha = sqrt(-r24 - (-2*r12 - 2*d2)*r22 - r14 + 2*d2*r12 - d2*d2) / (2*d*r1d)
    //val sinAlpha = sqrt(1 - cosAlpha*cosAlpha)
    val projectionLength = r1d * cosAlpha
    val height = r1d * sinAlpha

    val (orthX, orthY) = (x1 + projectionLength*dirX, y1 + projectionLength*dirY)
    val (heightX, heightY) = (-height*dirY, height*dirX)
    
    val (p1x, p1y) = (orthX + heightX, orthY + heightY)
    val (p2x, p2y) = (orthX - heightX, orthY - heightY)

    val res1 = Point(BigDecimal.valueOf(p1x).toBigInt, BigDecimal.valueOf(p1y).toBigInt)
    val res2 = Point(BigDecimal.valueOf(p2x).toBigInt, BigDecimal.valueOf(p2y).toBigInt)

    (res1, res2)
  }

  def mirror(p: Point, neighbour1: Point, neighbour2: Point): Point = {
    val dp = neighbour2 - neighbour1
    val dir = dp.normalized()
    val dp1 = (p - neighbour1).toPointD()
    val projectionLength = dir dot dp1
    val projection = dir * projectionLength
    val height = dp1 - projection
    //println(s"M: $p r.t. $neighbour1 - $neighbour2: dp = $dp, p = $projection, h = $height")

    (neighbour1.toPointD() + projection - height).round()
  }

  def mirrorAroundLine(solution: Vector[Point], p1: Point, p2: Point): Vector[Point] = {
    solution.map(p => mirror(p, p1, p2))
  }

  def foldAroundLine(solution: Vector[Point], p1: Point, p2: Point): Vector[Point] = {
    val dp = p2 - p1
    solution.map(p =>
      if ((dp cross (p - p1)) < 0) {
        mirror(p, p1, p2)
      } else {
        p
      }
    )
  }

  def calcCenter(solution: Vector[Point]): Point = {
    val xs = solution.map(_.x)
    val ys = solution.map(_.x)
    val avgX = (xs.min + xs.max) / 2
    val avgY = (ys.min + ys.max) / 2
    Point(avgX, avgY)
  }

  def mirrorX(solution: Vector[Point]) : Vector[Point] = {
    val maxX = solution.map(_.x).max
    solution.map(p => Point(maxX - p.x, p.y))
  }

  def mirrorY(solution: Vector[Point]) : Vector[Point] = {
    val maxY = solution.map(_.y).max
    solution.map(p => Point(p.x, maxY - p.y))
  }

  def transposeXY(solution: Vector[Point]): Vector[Point] = {
    val Point(avgX, avgY) = calcCenter(solution)
    solution.map(p => Point(avgX + p.y - avgY, avgY + p.x - avgX))
  }

  def foldInOne(figure: Figure, i: Int) : Option[Figure] = {
    val neighbours = figure.vertexNeighbours(i)
    if (neighbours.length == 2) {
      val p = figure.vertices(i)
      val p1 = figure.vertices(neighbours(0))
      val p2 = figure.vertices(neighbours(1))
      val q = mirror(p, p1, p2)
      Some(figure.copy(vertices = figure.vertices.updated(i, q)))
    } else {
      None
    }
  }

  def wobbleOne(problem: Problem, solution: Vector[Point], i: Int, delta: Int = 2) : Seq[Vector[Point]] = {
    val validator = new SolutionValidator(problem)
    val newPos = new ListBuffer[Vector[Point]]()
    val p = solution(i)
    for (dx <- -delta to delta) {
      for (dy <- -delta to delta) {
        if (dx != 0 || dy != 0) {
          val v = Point(p.x + dx, p.y + dy)
          val verts = solution.updated(i, v)
          val sol = Solution(verts, null)
          if (validator.validateEdgeLength(sol, Some(i))) {
            //println(f"Success: $p => $v")
            newPos += verts
          }
        }
      }
    }
    newPos.toSeq
  }
}
