package org.codingteam.icfpc2021

import java.awt.event.{ActionEvent, KeyEvent}
import javax.swing.{AbstractAction, Action}

package object visualizer {
  // TODO: move this helper function somewhere to an appropriate place
  def makeAction(name: String, callback: () => Unit, mnemonic: Option[Char] = None): AbstractAction = {
    val a = new AbstractAction(name) {
      override def actionPerformed(e: ActionEvent): Unit = callback()
    }
    mnemonic foreach { m =>
      a.putValue(Action.MNEMONIC_KEY, KeyEvent.getExtendedKeyCodeForChar(m))
    }
    a
  }
}
