import com.matthewk.domain.SKU._
import com.matthewk.domain._
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

    describe("addItems") {
      it("should add an item to the basket") {
        val basket = Basket.Empty
        val item = Item("A".toSKU, Price(150))
        val updatedBasket = basket.addItems(List(item))

        updatedBasket.items.size should be(1)
        updatedBasket.items.headOption.map(i => i should be(item)).getOrElse(fail)
      }

      it("should add items to a basket with existing items") {
        val basket = Basket.Empty
        val itemA = Item("A".toSKU, Price(10))
        val itemB = Item("B".toSKU, Price(5))

        val initialBasket = basket.addItems(List(itemA, itemA))
        initialBasket.items.size should be(2)

        val updatedBasket = initialBasket.addItems(List(itemB, itemB))
        updatedBasket.items should be(List(itemA, itemA, itemB, itemB))
      }
    }


  }

}
