package org.codingteam.icfpc2021.som

import java.awt.GridLayout
import javax.swing.border.TitledBorder
import javax.swing.{JLabel, JPanel, JTextField}

class SOMSolverOptionsPanel extends JPanel {

  private lazy val stepCountText = new JTextField()
  private lazy val startAlphaText = new JTextField()
  private lazy val startSigmaText = new JTextField()

  def options: SOMSolver.Options = {
    val r = for (stepCount <- stepCountText.getText.toIntOption;
                 startAlpha <- startAlphaText.getText.toDoubleOption;
                 startSigma <- startSigmaText.getText.toDoubleOption) yield
      SOMSolver.Options(stepCount, startAlpha, startSigma)
    r getOrElse SOMSolver.Options()
  }

  def options_=(opts: SOMSolver.Options): Unit = {
    stepCountText.setText(opts.stepCount.toString)
    startAlphaText.setText(opts.startAlpha.toString)
    startSigmaText.setText(opts.startSigma.toString)
  }

  private def init(): Unit = {
    setLayout(new GridLayout(-1, 2))

    def add(label: String, field: JTextField): Unit = {
      this.add(new JLabel(label))
      this.add(field)
    }

    add("n steps", stepCountText)
    add("start alpha", startAlphaText)
    add("start sigma", startSigmaText)
    this.options = SOMSolver.Options()

    this.setBorder(new TitledBorder("SOMSolver options"))
  }

  init()
}
