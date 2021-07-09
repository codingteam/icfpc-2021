package org.codingteam.icfpc2021.evaluator

import org.codingteam.icfpc2021.{Problem, Solution}

class SolutionEvaluator(problem: Problem) {

  /// Calculates the number of dislikes the solution will get.
  def evaluate(solution: Solution): BigInt = {
    val dislikes_count =
      problem.hole.view.map(hole_point =>
        solution.vertices.view.map(p => hole_point distanceSq p).min
      ).sum

    dislikes_count
  }

  def bestSolution(solutions: Seq[Solution]): Solution = {
    solutions minBy evaluate
  }
}

