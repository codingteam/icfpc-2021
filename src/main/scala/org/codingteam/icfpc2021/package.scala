package org.codingteam

package object icfpc2021 {

  case class Point(x: BigInt, y: BigInt) {
    override def toString: String = s"($x, $y)"
  }

  case class Edge(vertex1: Int, vertex2: Int) {
    override def toString: String = s"v$vertex1 - v$vertex2"
  }

  case class Figure(edges: Vector[Edge], vertices: Vector[Point])

  case class Problem(hole: Vector[Point], epsilon: BigInt, figure: Figure)

  case class Solution(vertices: Vector[Point])

}
