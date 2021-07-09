package org.codingteam.icfpc2021

case class Figure(edges: Vector[Point], vertices: Vector[Point])
case class Problem(hole: Vector[Point], epsilon: Int, figure: Figure)
