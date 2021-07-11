package org.codingteam.icfpc2021.solver


import java.awt
import java.awt.GridLayout
import javax.swing
import javax.swing.border.TitledBorder
import javax.swing.{JCheckBox, JLabel, JPanel, JTextField}

class SolutionOptimizerPanel extends JPanel {
  private lazy val useRotation = new JCheckBox("Use rotations")
  private lazy val useTranslation = new JCheckBox("Use translations")
  private lazy val useFold = new JCheckBox("Use folds")

  def options : Options = {
    Options(useRotation.isSelected, useTranslation.isSelected, useFold.isSelected)
  }

  def options_=(opts : Options): Unit = {
    useRotation.setSelected(opts.useRotations)
    useTranslation.setSelected(opts.useTranslations)
    useFold.setSelected(opts.useFolds)
  }

  def init() : Unit = {
    setLayout(new GridLayout(-1, 1))
    add(useRotation)
    add(useTranslation)
    add(useFold)
    this.options = Options(useRotations=true, useTranslations=true, useFolds=true)
    this.setBorder(new TitledBorder("Optimizer options"))
  }

  init()
}
