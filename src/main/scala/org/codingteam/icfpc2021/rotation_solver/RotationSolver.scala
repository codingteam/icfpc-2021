package org.codingteam.icfpc2021.rotation_solver

import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Json, Point, Solution}

import java.nio.file.{Files, Path}
import scala.math.{Pi, cos, sin}

object RotationSolver {
  /// Rotate by given number of radians.
  private def rotate_by(angle: Double, vertices: Vector[Point]): Vector[Point] = {
    val center_x = vertices.map(_.x.toDouble).sum / vertices.size.toDouble
    val center_y = vertices.map(_.y.toDouble).sum / vertices.size.toDouble

    vertices.map(p => {
      val x = center_x + (p.x.toDouble - center_x) * cos(angle) - (p.y.toDouble - center_y) * sin(angle)
      val y = center_x + (p.x.toDouble - center_x) * sin(angle) + (p.y.toDouble - center_y) * cos(angle)
      Point(x.toInt, y.toInt)
    })
  }

  def solve(problemFile: Path): Unit = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val evaluator = new SolutionEvaluator(problem)
    val validator = new SolutionValidator(problem)

    var start_rotation: Double = 0.0
    var rotation_window: Double = 2 * Pi
    val steps = 360

    for (_ <- 0 to 10) {
      println(s"Checking $steps angles from $start_rotation to ${start_rotation + rotation_window}")

      var best: Option[(Double, BigInt, Solution)] = None
      for (i <- 0 to steps) {
        val angle = start_rotation + i * rotation_window / steps

        val solution = Solution(rotate_by(angle, problem.figure.vertices), null)
        
        val dislikes = evaluator.evaluate(solution)
        best match {
          case None => best = Some((angle, dislikes, solution))
          case Some((_, best_dislikes, _)) =>
            if (best_dislikes > dislikes) {
              best = Some((angle, dislikes, solution))
            }
        }
      }

      best match {
        case None => println("No solution found")
        case Some((angle, dislikes, solution)) =>
          println(s"It's best to rotate by $angle radians to get $dislikes dislikes")
          if (validator.validate(solution)) {
            println("And the best thing is, that solution is valid!")
          } else {
            println("Unfortunately, that solution is invalid")
          }
          println(s"If you're still curious, the solution is: ${solution.vertices}")

          rotation_window /= 2.0
          start_rotation = angle - (rotation_window / 2.0)
      }
    }
  }
}
