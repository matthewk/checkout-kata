import domain._
import domain.SKU._
import org.scalatest.{FunSpec, Matchers}

class CheckoutSpec extends FunSpec with Matchers {

  describe("A Checkout") {
    it("should calculate the total price of a basket of items with no matching pricing rules") {
      val item = Item("A".toSKU, Price(2))

      Checkout(
        PricingRules.Empty
      ).total(Basket(item, item, item, item)).cost should be(
        Price(8)
      )
    }

    it("should calculate the total price of a basket of items with matching pricing rules") {
      val itemA = Item("A".toSKU, Price(3))
      val itemB = Item("B".toSKU, Price(5))

      Checkout(
        PricingRules(
          QuantityRule(itemA, 2, Price(5))
        )
      ).total(Basket(itemA, itemA, itemB)).cost should be(Price(10))
    }
  }
}
