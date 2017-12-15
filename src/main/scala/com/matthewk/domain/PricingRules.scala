package com.matthewk.domain

case class PricingRules private(rules: List[Rule]) {

  private val buildRules = (r: List[Rule]) => r.map(_.evaluate).reduce(_ compose _)

  /**
    * Given a basket of items apply the rules as a subtotal with any remaining items that did not have the rules applied
    *
    * @param basket A collection of selected Items
    * @return A domain.SubTotal of the items where rules applied and the remaining items
    */

  def applyTo(basket: Basket): SubTotal =
    buildRules(rules.sortBy(_.saving.amount)).apply(SubTotal(basket))

  def getSavingForItem(item: Item, quantity: Int): SubTotal =
    buildRules(rules.sortBy(_.saving.amount)).apply(SubTotal(Basket(List.fill(quantity)(item))))
}

object PricingRules {

  object Empty extends PricingRules(List(Rule.Empty))

  def apply(rules: Rule*) = new PricingRules(rules.toList)
}



sealed trait Rule {
  def evaluate: SubTotal => SubTotal

  def saving: Price
}

object Rule {

  object Empty extends Rule {
    override def evaluate: SubTotal => SubTotal = s => s

    override def saving: Price = Price.Zero
  }

}

case class QuantityRule(item: Item, quantity: Int, price: Price) extends Rule {

  override def evaluate: SubTotal => SubTotal = subtotal => {
    val (applicableItems, remainingItems) = subtotal.remainingItems.items.partition(Set(item))
    val (multiple: Int, remainder: Int) = (applicableItems.size / quantity, applicableItems.size % quantity)
    subtotal.copy(
      remainingItems = subtotal.remainingItems.copy(items = remainingItems ++ (1 to remainder).map(_ => item)),
      price = price.copy(amount = price.amount * multiple) + subtotal.price
    )
  }

  override def saving: Price = Price((item.price.amount * quantity) - price.amount)
}
