package models

case class Checkout(basket: Basket, rules: PricingRules) {

  lazy val total: Price = rules.applyTo(basket).asTotal

}
