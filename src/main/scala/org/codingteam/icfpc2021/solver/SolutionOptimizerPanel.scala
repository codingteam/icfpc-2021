package org.codingteam.icfpc2021.solver


import java.awt
import java.awt.GridLayout
import javax.swing
import javax.swing.border.TitledBorder
import javax.swing.{JCheckBox, JLabel, JPanel, JSpinner, JTextField, SpinnerNumberModel}

class SolutionOptimizerPanel extends JPanel {
  private lazy val useRotation = new JCheckBox("Use rotations")
  private lazy val useTranslation = new JCheckBox("Use translations")
  private lazy val useFold = new JCheckBox("Use folds")
  private lazy val translationDelta = new JSpinner(new SpinnerNumberModel(50, 1, 1000, 1))

  def options : Options = {
    val delta = translationDelta.getValue().asInstanceOf[Int]
    Options(useRotation.isSelected, useTranslation.isSelected, useFold.isSelected, delta)
  }

  def options_=(opts : Options): Unit = {
    useRotation.setSelected(opts.useRotations)
    useTranslation.setSelected(opts.useTranslations)
    useFold.setSelected(opts.useFolds)
    translationDelta.setValue(opts.translationDelta)
  }

  def init() : Unit = {
    setLayout(new GridLayout(-1, 1))
    add(useRotation)
    add(useTranslation)
    add(translationDelta)
    add(useFold)
    this.options = Options(useRotations=true, useTranslations=true, useFolds=true)
    this.setBorder(new TitledBorder("Optimizer options"))
  }

  init()
}
