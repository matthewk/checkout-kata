import com.matthewk.domain.SKU._
import com.matthewk.domain._
import org.scalatest.{FunSpec, Matchers}

class PricingRulesSpec extends FunSpec with Matchers {

  describe("A PricingRuleSet") {

    it("should apply a pricing rule for an item") {
      val item = Item("A".toSKU, Price(7))

      PricingRules(
        QuantityRule(item, 2, Price(9))
      ).applyTo(
        Basket(item, item)
      ) should be(
        SubTotal(Basket.Empty, Price(9))
      )
    }

    it("should provide a total price having applied price rules where applicable") {
      val itemA = Item("A".toSKU, Price(5))
      val itemB = Item("B".toSKU, Price(8))

      PricingRules(
        QuantityRule(itemA, 2, Price(6)),
        QuantityRule(itemB, 3, Price(11))
      ).applyTo(
        Basket(
          itemA, itemB, itemA, itemB, itemA, itemB, itemB, itemB
        )
      ).asTotal should be(
        Price(38)
      )
    }

    it("should provide a total price having applied multiple price rules where applicable") {
      val itemA = Item("A".toSKU, Price(5))
      val itemB = Item("B".toSKU, Price(8))

      PricingRules(
        QuantityRule(itemA, 2, Price(6)),
        QuantityRule(itemB, 3, Price(11)),
        QuantityRule(itemA, 3, Price(12))
      ).applyTo(
        Basket(
          itemA, itemB, itemA, itemB, itemA, itemB, itemB, itemB, itemA, itemA
        )
      ).asTotal should be(
        Price(44)
      )
    }
  }
}
