package org.codingteam.icfpc2021.evaluator

import org.codingteam.icfpc2021.{Problem, Solution, Point}

class SolutionEvaluator(problem: Problem) {
  private def distance(a: Point, b: Point): BigInt = {
    (a.x - b.x).pow(2) + (a.y - b.y).pow(2)
  }

  /// Calculates the number of dislikes the solution will get.
  def evaluate(solution: Solution): BigInt = {
    val dislikes_count =
      problem.hole.map(hole_point =>
        solution.vertices.map(p => distance(hole_point, p)).min
      ).sum

    dislikes_count
  }
}

