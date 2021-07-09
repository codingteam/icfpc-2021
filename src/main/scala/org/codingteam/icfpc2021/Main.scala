package org.codingteam.icfpc2021

object Main extends App {
  args match {
    case Array("visualizer") => Visualizer.show()
    case _ => println(
      """Possible arguments:
        |
        |visualizer
        |  Will show visualizer.""".stripMargin)
  }
}
