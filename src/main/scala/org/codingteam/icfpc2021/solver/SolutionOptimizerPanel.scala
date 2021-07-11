package org.codingteam.icfpc2021.solver


import java.awt
import java.awt.GridLayout
import javax.swing
import javax.swing.border.TitledBorder
import javax.swing.{JCheckBox, JLabel, JPanel, JTextField}

class SolutionOptimizerPanel extends JPanel {
  private lazy val useRotation = new JCheckBox("Use rotations")
  private lazy val useTranslation = new JCheckBox("Use translations")

  def options : Options = {
    Options(useRotation.isSelected, useTranslation.isSelected)
  }

  def options_=(opts : Options): Unit = {
    useRotation.setSelected(opts.useRotations)
    useTranslation.setSelected(opts.useTranslations)
  }

  def init() : Unit = {
    setLayout(new GridLayout(-1, 1))
    add(useRotation)
    add(useTranslation)
    this.options = Options(useRotations=true, useTranslations=true)
    this.setBorder(new TitledBorder("Optimizer options"))
  }

  init()
}
