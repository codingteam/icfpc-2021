package org.codingteam.icfpc2021.som

import java.awt.GridLayout
import javax.swing.border.TitledBorder
import javax.swing.{JLabel, JPanel, JTextField}

class SOMSolverOptionsPanel extends JPanel {

  private lazy val stepCountText = new JTextField()
  private lazy val startAlphaText = new JTextField()
  private lazy val startSigmaText = new JTextField()
  private lazy val edgeForceKText = new JTextField()
  private lazy val edgeStepsText = new JTextField()


  def options: SOMSolver.Options = {
    val r = for (stepCount <- stepCountText.getText.toIntOption;
                 startAlpha <- startAlphaText.getText.toDoubleOption;
                 startSigma <- startSigmaText.getText.toDoubleOption;
                 edgeForceK <- edgeForceKText.getText.toDoubleOption;
                 edgeSteps <- edgeStepsText.getText.toIntOption) yield
      SOMSolver.Options(stepCount, startAlpha, startSigma, edgeForceK, edgeSteps)
    r getOrElse SOMSolver.Options()
  }

  def options_=(opts: SOMSolver.Options): Unit = {
    stepCountText.setText(opts.stepCount.toString)
    startAlphaText.setText(opts.startAlpha.toString)
    startSigmaText.setText(opts.startSigma.toString)
    edgeForceKText.setText(opts.edgeForceK.toString)
    edgeStepsText.setText(opts.edgeStepsPerHoleStep.toString)
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
    add("edge force k", edgeForceKText)
    add("edge steps per step", edgeStepsText)
    this.options = SOMSolver.Options()

    this.setBorder(new TitledBorder("SOMSolver options"))
  }

  init()
}
