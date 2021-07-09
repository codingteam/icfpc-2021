package org.codingteam.icfpc2021

import scala.swing.Frame

object Visualizer {
  def show(): Unit = {
    new Frame {
      title = "visualizer"

      pack()
      centerOnScreen()
      open()
    }
  }
}
