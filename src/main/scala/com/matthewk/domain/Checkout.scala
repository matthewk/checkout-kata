package com.matthewk.domain

/**
  * The checkout pulls together both parts of functionality in the Basket and the Pricing Rules
  * @param rules A collection of Pricing Rules to apply to the items in the basket
  */
case class Checkout(rules: PricingRules) {
  def total(basket: Basket): Total = Total(rules.applyTo(basket).asTotal, basket.totalPrice)
}


