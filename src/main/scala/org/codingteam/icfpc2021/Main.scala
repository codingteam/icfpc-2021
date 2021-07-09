package org.codingteam.icfpc2021

import java.nio.file.Path

object Main extends App {
  args match {
    case Array("visualizer", path) => Visualizer.show(Path.of(path))
    case _ => println(
      """Possible arguments:
        |
        |visualizer <path.json>
        |  Will show visualizer for <path>.""".stripMargin)
  }
}
