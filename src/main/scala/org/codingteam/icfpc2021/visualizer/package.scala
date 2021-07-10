package org.codingteam.icfpc2021

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

package object visualizer {
  // TODO: move this helper function somewhere to an appropriate place
  def makeAction(name: String, callback: () => Unit): AbstractAction = new AbstractAction(name) {
    override def actionPerformed(e: ActionEvent): Unit = callback()
  }
}
