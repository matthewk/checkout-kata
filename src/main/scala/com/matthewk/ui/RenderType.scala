package com.matthewk.ui

sealed trait RenderType {
  type OutputType

  def render(input: String): OutputType
  def clear: OutputType
}

trait ConsoleRender extends RenderType {
  override type OutputType = Unit

  override def render(input: String): Unit = {
    clear
    println(input)
  }

  override def clear: Unit = print("\u001bc")
}