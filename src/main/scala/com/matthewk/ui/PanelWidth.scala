package com.matthewk.ui

sealed trait PanelWidth {
  def value: Int
}

object PanelWidth {

  case object Width50 extends PanelWidth {
    val value = 25
  }

  case object Width100 extends PanelWidth {
    val value = 40
  }

  case object Width200 extends PanelWidth {
    val value = 70
  }
}