package com.matthewk.ui.console

import com.matthewk.ui.Stylesheet

import scala.io.AnsiColor._

trait ConsoleStylesheet extends Stylesheet {
  val yellow   : String => String = child => s"$YELLOW$child"
  val red      : String => String = child => s"$RED$child"
  val green    : String => String = child => s"$GREEN$child"
  val bold     : String => String = child => s"$BOLD$child"
  val reversed : String => String = child => s"$REVERSED$child"
  val underline: String => String = child => s"$UNDERLINED$child"

  val reset   : String => String = child => s"$RESET$child$RESET"
  val errors  : String => String = child => reset(red(child))
  val success: String => String = child => reset(green(child))
  val title   : String => String = child => reset(bold(child))
  val appTitle: String => String = child => reset(yellow(child))
  val menuLine: String => String = child => reset(bold(child))
  val menuItem: String => String = child => reset(yellow(bold(reversed(s"(${child.head})${child.tail}"))))
}