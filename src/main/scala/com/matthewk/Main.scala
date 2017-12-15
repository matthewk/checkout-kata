package com.matthewk

import com.matthewk.domain._
import com.matthewk.ui._
import com.matthewk.ui.console.{ConsoleMenuActions, ConsoleViews}

import scala.io.StdIn.readLine

object Main extends App with ConsoleMenuActions {

  private val catalogue = InitialData.catalogue

  private val checkout = Checkout(InitialData.pricingRules)

  val applicationParams: ApplicationParams = ApplicationParams(checkout, Basket.Empty, catalogue)

  override def menuActions = List(
    ViewAddItemsMenu,
    ViewBasketMenu,
    ViewCatalogueMenu,
    ViewPricingRulesMenu,
    EmptyBasketMenu,
    QuitAppMenu
  )

  private val unknownAction = (command: String) =>
    new UnknownAction with UnknownHandler with ConsoleViews {
      override def cmd: String = command
    }

  val defaultAction: Handler = new ViewBasketAction with ViewBasketHandler with ConsoleViews

  def menuLabels = menuActions.map(_.name)


  def awaitInput(ap: ApplicationParams): Unit = {
    val input :: args = readLine.split("[ \t]+").toList

    menuActions.find(_.matches(input))
      .map(a => a.handle(ap, menuLabels, args))
      .getOrElse(unknownAction(input).handle(ap, menuLabels, args)) match {
      case ContinueState(params) => awaitInput(params)
      case QuitState             => println("goodbye")
    }
  }

  defaultAction.handle(applicationParams, menuLabels)
  print("?")
  awaitInput(applicationParams)

}