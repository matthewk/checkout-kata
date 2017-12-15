package com.matthewk.ui

trait Stylesheet {
  def yellow:     String => String
  def red:        String => String
  def green:      String => String
  def bold:       String => String
  def reversed:   String => String
  def underline:  String => String
  def reset:      String => String
  val errors :    String => String
  val success:    String => String

  def title:    String => String
  def appTitle: String => String
  def menuLine: String => String
  def menuItem: String => String

  def center(input: String, width: Int, formatting: String => String): String = {
    val midPoint: Int = (width - input.length) / 2

    (1 to midPoint).map(_ => " ").mkString + formatting(input)
  }

  val bar: Int => String = (barWidth: Int) => (0 to barWidth).toList.map(_ => "-").mkString("")

  def panel(titleText: String, child: String, width: PanelWidth = PanelWidth.Width50): String = {
    Seq(
      bar(width.value),
      title(titleText),
      bar(width.value),
      child,
      bar(width.value)
    ).mkString("\n")
  }
}


