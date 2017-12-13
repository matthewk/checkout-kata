import models._
import org.scalatest.{FunSpec, Matchers}
import models.SKU._
import models.SKU._

class BasketSpec extends FunSpec with Matchers {

  describe("A Basket") {
    it("should calculate the total price of a basket of items") {
      val item = Item("A".toSKU, Price(5))

      Checkout(
        Basket(item, item, item, item),
        PricingRules.Empty
      ).total should be (
        Price(20)
      )
    }

    describe("addItem") {
      it("should add an item to the basket") {
        val basket = Basket.Empty
        val item = Item("A".toSKU, Price(150))
        val updatedBasket = basket.addItem(item)

        updatedBasket.items.size should be(1)
        updatedBasket.items.headOption.map(i => i should be(item)).getOrElse(fail)
      }
    }

    describe("addItemQuantity") {
      it("should add a quantity of an item to the basket") {
        val basket = Basket.Empty
        val item = Item("B".toSKU, Price(190))
        val updatedBasket = basket.addItemQuantity(3, item)

        updatedBasket.items.size should be(3)
        updatedBasket.items.headOption.map(i => i should be(item)).getOrElse(fail)
      }
    }

    describe("addItems") {

      it("should add a number of items with their quantities to the basket") {
        val basket = Basket.Empty
        val itemB = Item("B".toSKU, Price(190))
        val itemC = Item("C".toSKU, Price(2316))

        val items = List(
          (2, itemB),
          (3, itemC)
        )
        val updatedBasket = basket.addItems(items)

        updatedBasket.items.size should be(5)
        updatedBasket.items.headOption.map(i => i should be(itemB)).getOrElse(fail)
      }
    }

    it("should provide a readable overview of its contents") {
      val basket = Basket.Empty
      val itemB = Item("B".toSKU, Price(1.90))
      val itemC = Item("C".toSKU, Price(23.16))

      val items = List(
        (2, itemB),
        (3, itemC)
      )
      val updatedBasket = basket.addItems(items)
      val printValue = updatedBasket.toString

      print(printValue)

      printValue.length should be > 1

    }
  }

}
