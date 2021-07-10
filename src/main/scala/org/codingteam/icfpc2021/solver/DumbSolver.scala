package org.codingteam.icfpc2021.solver

import java.nio.file.{Files, Path}

import math.{sin, cos, Pi}
import scala.collection.mutable

import org.codingteam.icfpc2021.{Json, Point, Problem, Solution}
import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator

object DumbSolver {
  def brezenhem(radius: BigInt) : Seq[Point] = {
    val result = new mutable.ListBuffer[Point]()
    val center = Point(0,0)
    val r2 = radius*radius
    var x = 0
    var y = radius
    var delta = 1 - 2*radius
    var error = BigInt(0)
    while (y >= x) {
      val p = Point(x, y)
      if ((p distanceSq center) == r2) {
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
}