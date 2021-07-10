package org.codingteam.icfpc2021

import java.awt.event.{ActionEvent, KeyEvent}
import java.awt.Color
import java.awt.geom.Arc2D
import javax.swing.{AbstractAction, Action}
import scala.swing.Graphics2D

package object visualizer {
  def makeAction(name: String, callback: () => Unit, mnemonic: Option[Char] = None): AbstractAction = {
    val a = new AbstractAction(name) {
      override def actionPerformed(e: ActionEvent): Unit = callback()
    }
    mnemonic foreach { m =>
      a.putValue(Action.MNEMONIC_KEY, KeyEvent.getExtendedKeyCodeForChar(m))
    }
    a
  }

  def drawArc(
    g: Graphics2D,
    center: (Int, Int),
    direction: (Int, Int),
    r0: Int, r1: Int,
  ): Unit = {
    val angle = math.toDegrees(math.atan2(
      center._2 - direction._2,
      -center._1 + direction._1
    ))
    val angleD = 15

    g.setColor(new Color(0.0f, 0.0f, 1.0f, 0.5f))
    g.draw(new Arc2D.Double(
      center._1 - r0,
      center._2 - r0,
      r0*2, r0*2,
      angle - angleD, angleD*2, Arc2D.OPEN,
    ))

    g.setColor(new Color(0.0f, 1.0f, 0.0f, 0.5f))
    g.draw(new Arc2D.Double(
      center._1 - r1,
      center._2 - r1,
      r1*2, r1*2,
      angle - angleD, angleD*2, Arc2D.OPEN,
    ))
  }
}
