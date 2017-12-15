import domain.SKU._
import domain._
import org.scalatest.{FunSpec, Matchers}

class BasketSpec extends FunSpec with Matchers {

  describe("A Basket") {
    it("should calculate the total price of a basket of items") {
      val item = Item("A".toSKU, Price(5))

      Checkout(
        PricingRules.Empty
      ).total(Basket(item, item, item, item)).cost should be(
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

  }

}
