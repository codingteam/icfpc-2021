package org.codingteam.icfpc2021

case class Point(x: Int, y: Int) {
  override def toString: String = s"($x, $y)"
}

case class VertexPair(vertex1: Int, vertex2: Int) {
  override def toString: String = s"v$vertex1 - v$vertex2"
}

case class Figure(edges: Vector[VertexPair], vertices: Vector[Point])
case class Problem(hole: Vector[Point], epsilon: Int, figure: Figure)
