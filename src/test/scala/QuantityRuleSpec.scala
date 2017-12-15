import com.matthewk.domain.SKU._
import com.matthewk.domain._
import org.scalatest.{FunSpec, Matchers}


class QuantityRuleSpec extends FunSpec with Matchers {
  describe("A Pricing Rule") {

    it("should calculate the correct domain.SubTotal for a basket with matching items") {
      val item = Item("A".toSKU, Price(10))

      QuantityRule(item, 3, Price(20))
        .evaluate(
          SubTotal(Basket(item, item, item, item))
        ) should be(
        SubTotal(Basket(List(item)), Price(20))
      )
    }

    it("should calculate the correct domain.SubTotal for a basket with multiple matching items") {
      val item = Item("A".toSKU, Price(10))

      QuantityRule(item, 2, Price(15))
        .evaluate(
          SubTotal(Basket(item, item, item, item, item))
        ) should be(
        SubTotal(Basket(List(item)), Price(30))
      )
    }

    it("should calculate the correct domain.SubTotal for a basket with exact multiple matching items") {
      val item = Item("A".toSKU, Price(10))

      QuantityRule(item, 2, Price(15))
        .evaluate(
          SubTotal(Basket(item, item, item, item))
        ) should be(
        SubTotal(Basket(), Price(30))
      )
    }

    it("should calculate the correct domain.SubTotal for a basket with no matching items") {
      val nonMatchingItem = Item("A".toSKU, Price(10))
      val item = Item("B".toSKU, Price(5))

      QuantityRule(item, 3, Price(20))
        .evaluate(
          SubTotal(Basket(nonMatchingItem, nonMatchingItem, nonMatchingItem, nonMatchingItem))
        ) should be(
        SubTotal(
          Basket(nonMatchingItem, nonMatchingItem, nonMatchingItem, nonMatchingItem),
          Price(0)
        )
      )
    }

    it("should be able to evaluate on a domain.SubTotal") {
      val nonMatchingItem = Item("A".toSKU, Price(10))
      val item = Item("B".toSKU, Price(5))

      QuantityRule(item, 3, Price(20))
        .evaluate(
          SubTotal(Basket(nonMatchingItem, nonMatchingItem, nonMatchingItem, nonMatchingItem))
        ) should be(
        SubTotal(
          Basket(nonMatchingItem, nonMatchingItem, nonMatchingItem, nonMatchingItem),
          Price(0)
        )
      )
    }

  }
}
