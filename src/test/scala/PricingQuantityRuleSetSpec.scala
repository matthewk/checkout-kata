import org.scalatest.FunSpec

class PricingQuantityRuleSetSpec extends FunSpec {
  describe("A PricingRuleSet") {
    it("should have a field 'selection' that stores a function to group items so that pricing rules can be applied")(pending)

    it("should have a field 'rules' to store this week's prices")(pending)

    it("should return a price based on its contained rules for a matched quantity of items with an associated price rule")(pending)

    it("should return a price based on its contained rules for a matching quantity of items with an associated price rule plus a price for the unmatched quantity of items")(pending)

    it("should return a price based on quantity multiplied by item individual price")(pending)
  }

}
