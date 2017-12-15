package com.matthewk.ui.console

import com.matthewk. ItemValidatorNel
import com.matthewk.ui._

trait ConsoleMenuActions extends MenuActions {

  def menuActions: List[Action with Handler]

  case object ViewAddItemsMenu extends AddItemsAction with ConsoleViews with AddItemsHandler with ItemValidatorNel
  case object ViewBasketMenu extends ViewBasketAction with ConsoleViews with ViewBasketHandler
  case object ViewCatalogueMenu extends ViewCatalogueAction with ConsoleViews with ViewCatalogueHandler
  case object ViewPricingRulesMenu extends ViewPricingRulesAction with ConsoleViews with ViewPricingRulesHandler
  case object EmptyBasketMenu extends EmptyBasketAction with ConsoleViews with EmptyBasketHandler
  case object QuitAppMenu extends QuitAppAction
}