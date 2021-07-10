package org.codingteam.icfpc2021.solver

import java.nio.file.{Files, Path}

import math.{sin, cos, Pi, sqrt, abs}
import scala.collection.mutable

import org.codingteam.icfpc2021.{Json, Point, Problem, Solution}
import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator

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
      if (abs((p distanceSq center).toDouble - r2) < epsilon) {
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
}
