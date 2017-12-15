import Main.MenuItem
import cats.implicits._
import domain._

import scala.io.AnsiColor._
import scala.io.StdIn._
import domain.SKU._

object Main extends App
  with Elements {

  import ItemValidatorNel._

  // Update product catalogue here
  private val catalogue: Catalogue =
    Catalogue(
      Item("A".toSKU, Price(50)),
      Item("B".toSKU, Price(30)),
      Item("C".toSKU, Price(20)),
      Item("D".toSKU, Price(15))
    )

  private val pricingRules = (for {
    r1 <- validateQuantityRule("A", 3, BigDecimal(130), catalogue.items.values.toList).toEither
    r2 <- validateQuantityRule("B", 2, BigDecimal(45), catalogue.items.values.toList).toEither
  } yield PricingRules(r1, r2))
    .fold(
      err => {
        println(err.map(_.e).toList.mkString(","))
        PricingRules.Empty
      }, pr => pr)


  def catalogueView =
    panel(
      center("Catalogue", PanelWidth.Width200, underline),
      catalogue.items.mkString("\n"),
      PanelWidth.Width200
    )

  val maybeShowBasketContents = (checkout: Checkout, basket: Basket) =>
    if (basket.items.isEmpty) {
      """
        |Your basket is empty.
        |You can add items by entering 'add'
        |with any nunber of SKU seperated
        |by a space
      """.stripMargin
    } else {
      basket.renderWithSavings(checkout.rules)
    }

  def center(input: String, width: PanelWidth, formatting: String => String): String = {
    def midPoint: Int = (width.value - input.length) / 2

    (1 to midPoint).map(_ => " ").mkString + formatting(input)
  }

  def basketView(checkout: Checkout, basket: Basket) =
    panel(
      center("Your Basket", PanelWidth.Width200, underline),
      Seq(
        maybeShowBasketContents(checkout, basket),
        bar(PanelWidth.Width200.value),
        checkout.total(basket).toString
      ).mkString("\n"),
      PanelWidth.Width200
    )

  def offerView(pricingRules: PricingRules) =
    panel(
      center("Current Offers", PanelWidth.Width200, underline),
      pricingRules.rules.sortBy(_.saving.amount)(Ordering[BigDecimal].reverse).map {
        case QuantityRule(item, quantity, price) =>
          s"Buy $quantity $item for $price *** SAVING ${item.price.copy(amount = item.price.amount * quantity) - price}"
        case _                                   => ""
      }.mkString("\n"),
      PanelWidth.Width200
    )

  val menu = Seq(
    MenuItem.ShowBasket,
    MenuItem.AddItems,
    MenuItem.MenuCatalogue,
    MenuItem.ShowOffers,
    MenuItem.MenuEmpty,
    MenuItem.Quit
  )

  val checkout = Checkout(pricingRules)

  // Program
  println(clearScreen)
  println(appTitle("Checkout Kata"))
  println(catalogueView)

  val error = (err: List[DomainValidation]) =>
    println(reset(bold(red(err.map(a => a.message).mkString("\n")))))

  val updatedBasket = (basket: Basket) =>
    println(
      basketView(checkout, basket)
    )

  case class ExecutionParams(input: List[String], checkout: Checkout, basket: Basket)

  sealed trait MenuItem {
    def label: String
    def description: String
    def execution: ExecutionParams => Option[(Checkout, Basket)]
    def matcher: String => Boolean = (input: String) => input.equalsIgnoreCase(label) || input.equalsIgnoreCase(label.take(1))
    def apply(executionParams: ExecutionParams): Unit = {
      println(clearScreen)
      println(appTitle("Checkout Kata"))
      execution(executionParams) match {
        case Some((c, b)) => waitInput(c, b)
        case _            => println("*******")
      }

    }
  }

  /**
    * The menu items represent a set of types covering the interactions with the application, this allows us to
    * abstract away repeated implementation
    */
  object MenuItem {
    
    
    case object AddItems extends MenuItem {
      val label: String = "add"

      val description: String = "Add Item"

      val execution: ExecutionParams => Option[(Checkout, Basket)] = executionParams => {
        val result = ItemValidatorNel.validateAddItems(executionParams.input.map(_.toUpperCase), catalogue)
        result.fold(
          err => {
            updatedBasket(executionParams.basket)
            error(err.toList)
            Some((executionParams.checkout, executionParams.basket))
          },
          i => {
            val newBasket = executionParams.basket.addItems(i)
            updatedBasket(newBasket)
            Some((checkout, newBasket))
          }
        )
      }
    }

    case object MenuCatalogue extends MenuItem {
      override val label: String = "catalogue"

      override val description: String = "view available items"

      override val execution: ExecutionParams => Option[(Checkout, Basket)] = executionParams => {
        println(catalogueView)
        Some((executionParams.checkout, executionParams.basket))
      }
    }

    case object MenuEmpty extends MenuItem {
      override val label: String = "empty"

      override val description: String = "empty basket"

      override val execution: ExecutionParams => Option[(Checkout, Basket)] = executionParams => {
        val newBasket = Basket.Empty
        println(basketView(executionParams.checkout, newBasket))
        Some((executionParams.checkout, newBasket))
      }

    }

    case object ShowBasket extends MenuItem {
      override val label: String = "show"

      override val description: String = "show basket"

      override val execution: ExecutionParams => Option[(Checkout, Basket)] = executionParams => {
        println(basketView(executionParams.checkout, executionParams.basket))
        Some((executionParams.checkout, executionParams.basket))
      }
    }

    case object ShowOffers extends MenuItem {
      override val label: String = "offers"

      override val description: String = "show offers"

      override val execution: ExecutionParams => Option[(Checkout, Basket)] = executionParams => {
        println(offerView(executionParams.checkout.rules))
        Some((executionParams.checkout, executionParams.basket))
      }
    }

    case object Quit extends MenuItem {
      override val label: String = "quit"

      override val description: String = "leave the store"

      override val execution: ExecutionParams => Option[(Checkout, Basket)] = executionParams => {
        println("Thank you! Goodbye :)")
        None
      }
    }

    case object Unknown extends MenuItem {
      override val label: String = "Unknown"

      override val description: String = ""

      override val execution: ExecutionParams => Option[(Checkout, Basket)] = executionParams => {
        println(basketView(executionParams.checkout, executionParams.basket))
        println(yellow(s"I didn't understand the command ${executionParams.input.mkString(" ")}"))
        Some((executionParams.checkout, executionParams.basket))
      }
    }

    def render(menuItems: Seq[MenuItem]): Unit = println(menuLine(menuItems.map(menuItem).mkString(reset("  |  "))))
  }


  def waitInput(checkout: Checkout, basket: Basket = Basket.Empty): Unit = {
    MenuItem.render(menu)
    print(reset(bold("?")))
    readLine.split("[ \t]+").toList match {
      case input :: x if MenuItem.ShowBasket.matcher(input) =>
        MenuItem.ShowBasket(ExecutionParams(x, checkout, basket))

      case input :: x if MenuItem.MenuCatalogue.matcher(input) =>
        MenuItem.MenuCatalogue(ExecutionParams(x, checkout, basket))

      case input :: x if MenuItem.AddItems.matcher(input) =>
        MenuItem.AddItems(ExecutionParams(x, checkout, basket))

      case input :: x if MenuItem.MenuEmpty.matcher(input) =>
        MenuItem.MenuEmpty(ExecutionParams(x, checkout, basket))

      case input :: x if MenuItem.ShowOffers.matcher(input) =>
        MenuItem.ShowOffers(ExecutionParams(x, checkout, basket))

      case input :: x if MenuItem.Quit.matcher(input) =>
        MenuItem.Quit(ExecutionParams(x, checkout, basket))

      case cmd =>
        MenuItem.Unknown(ExecutionParams(cmd, checkout, basket))
    }
  }

  waitInput(checkout)

}

trait Elements {


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

  val clearScreen: String           = "\u001bc"
  val yellow     : String => String = child => s"$YELLOW$child"
  val red        : String => String = child => s"$RED$child"
  val bold       : String => String = child => s"$BOLD$child"
  val reversed   : String => String = child => s"$REVERSED$child"
  val underline  : String => String = child => s"$UNDERLINED$child"

  val reset   : String => String   = child => s"$RESET$child$RESET"
  val title   : String => String   = child => reset(bold(child))
  val appTitle: String => String   = child => reset(yellow(child))
  val menuLine: String => String   = child => reset(bold(child))
  val menuItem: MenuItem => String = child => reset(yellow(bold(s"(${child.label.head})${child.label.tail}")))

  val bar: Int => String = (barWidth: Int) => (0 to barWidth).toList.map(a => "-").mkString("")

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
