package com.matthewk.ui

trait MenuActions {
  def menuActions: List[Action with Handler]
}

sealed trait Action {
  self: Handler =>

  def name: String

  val matches: String => Boolean = (toMatch) =>
    name.equalsIgnoreCase(toMatch) || name.take(1).equalsIgnoreCase(toMatch.take(1))
}

trait ViewBasketAction extends Action with Handler {
  val name: String = "basket"
}

trait ViewCatalogueAction extends Action with Handler {
  val name: String = "catalogue"
}

trait AddItemsAction extends Action with Handler {
  val name: String = "add"
}

trait EmptyBasketAction extends Action with Handler {
  val name: String = "empty"
}

trait ViewPricingRulesAction extends Action with Handler {
  val name: String = "offers"
}

trait QuitAppAction extends Action with Handler {
  val name: String = "quit"
  override def handle(ap: ApplicationParams, menuItems: List[String] = List.empty[String], args: List[String]): ApplicationState = QuitState
}

trait UnknownAction extends Action with Handler {
  val name: String = ""
}