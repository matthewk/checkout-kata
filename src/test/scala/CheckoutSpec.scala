import models._
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}
import models.SKU._

class CheckoutSpec extends FunSpec with Matchers {

  describe("A Checkout") {
    it("should calculate the total price of a basket of items with no matching pricing rules") {
      val item = Item("A".toSKU, Price(2))

      Checkout(
        Basket(item, item, item, item),
        PricingRules.Empty
      ).total should be (
        Price(8)
      )
    }

    it("should calculate the total price of a basket of items with matching pricing rules") {
      val itemA = Item("A".toSKU, Price(3))
      val itemB = Item("B".toSKU, Price(5))

      Checkout(
        Basket(itemA, itemA, itemB),
        PricingRules(
          QuantityRule(itemA, 2, Price(5))
        )
      ).total should be (Price(10))
    }
  }
}
