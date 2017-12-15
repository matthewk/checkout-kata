package com.matthewk.ui.console

import com.matthewk.domain._
import com.matthewk.ui.{ApplicationParams, ConsoleRender, PanelWidth, Views}

trait ConsoleViews extends Views with ConsoleRender with ConsoleStylesheet {
  private def savingsLabel(item: Item, quantity: Int, price: Price): String =
    s"Buy $quantity $item for $price *** SAVING ${item.price.copy(amount = item.price.amount * quantity) - price}"

  private def maybeShowBasketContents(checkout: Checkout, basket: Basket): String =
    if (basket.items.isEmpty) {
      """
        |Your basket is empty.
        |You can add items by entering 'add'
        |with any number of SKU separated
        |by a space
      """.stripMargin
    } else {
      basket.renderWithSavings(checkout.rules)
    }

  private def screen(title: String, content: String, messages: String, menuItems: List[String]): String =
    Seq(
      bold(yellow("Kata Store")),
      panel(center(title, PanelWidth.Width200.value, underline), content, PanelWidth.Width200),
      messages,
      menuBar(menuItems)
    ).mkString("\n")

  def errorsLabel(err: List[DomainValidation]) =
    reset(bold(red(err.map(a => a.message).mkString("\n"))))

  def successLabel(msg: String) =
    reset(bold(green(msg)))

  private def messages(success: Option[String], errors: Option[List[DomainValidation]]): String = {
    Seq(success.map(s => successLabel(s)), errors.map(e => errorsLabel(e))).flatten.mkString("\n")
  }

  override def showBasketView(ap: ApplicationParams, menuOptions: List[String], success: Option[String], errors: Option[List[DomainValidation]]): Unit = {
    render(
      screen(
        "Your Basket",
        Seq(
          maybeShowBasketContents(ap.checkout, ap.basket),
          bar(PanelWidth.Width200.value),
          ap.checkout.total(ap.basket).toString
        ).mkString("\n"),
        messages(success, errors),
        menuOptions
      )
    )
  }

  override def showCatalogueView(ap: ApplicationParams, menuOptions: List[String]): Unit = {
    render(
      screen(
        "Our Available Items",
        ap.catalogue.items.mkString("\n"),
        "",
        menuOptions
      )
    )
  }

  override def showOffersView(ap: ApplicationParams, menuOptions: List[String]): Unit = {
    render(
      screen(
        "Current Offers",
        ap.checkout.rules.rules.sortBy(_.saving.amount)(Ordering[BigDecimal].reverse)
          .map {
            case QuantityRule(item, quantity, price) => savingsLabel(item, quantity, price)
            case _                                   => ""
          }.mkString("\n"),
        "",
        menuOptions
      )
    )
  }

  override def showAddItemsView(
    ap: ApplicationParams,
    menuOptions: List[String],
    success: Option[String],
    errors: Option[List[DomainValidation]]
  ): Unit = showBasketView(ap, menuOptions, success, errors)

  override def menuBar(items: List[String]): String = menuLine(items.map(menuItem).mkString(reset("  |  ")))
}

