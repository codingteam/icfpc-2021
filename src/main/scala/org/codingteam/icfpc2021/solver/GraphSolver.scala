package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.rotation_solver.RotationSolver
import org.codingteam.icfpc2021.{Edge, Figure, Point, Problem, Solution}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GraphSolver(problem: Problem) {
  def tryBreakGraphByRemovingEdge(edge: Edge): Option[(Vector[Int], Vector[Int])] = {
    val excluded = List(edge.vertex1, edge.vertex2).toSet
    val components = findComponents(excluded)
    if (components.length == 2) {
      val c1 = components(0)
      val c2 = components(1)
      Some((c1, c2))
    } else {
      None
    }
  }

  def findComponents(excludeVerts: Set[Int]): Seq[Vector[Int]] = {
    val nVerts = problem.figure.vertices.length
    val nExcluded = excludeVerts.size
    val allVerts = problem.figure.vertices.indices
    val visited = mutable.TreeMap[Int,Int]()
    var componentNumber = 0
    val components = mutable.Map[Int,Vector[Int]]().withDefaultValue(Vector[Int]())

    while (visited.size + nExcluded < nVerts) {
      val start = allVerts.filter(i => ! visited.contains(i) && ! excludeVerts.contains(i))(0)
      //println(s"C.$componentNumber: start from $start")
      runDfs(start, visited, componentNumber, excludeVerts)
      componentNumber += 1
    }

    visited.keys.foreach(v => {
      val component = visited(v)
      components(component) = components(component) :+ v
    })

    components.values.toSeq
  }

  private def runDfs(start: Int, visited: mutable.TreeMap[Int,Int], component: Int, excludeVerts: Set[Int]): Unit = {
    if (! excludeVerts.contains(start)) {
      //println(s"C.$component: visit v.$start")
      visited(start) = component
      problem.figure.vertexNeighbours(start).foreach(v => {
        //println(s"C.$component: check $start -> $v")
        if (!visited.contains(v)) {
          runDfs(v, visited, component, excludeVerts)
        }
      })
    }
  }
}
