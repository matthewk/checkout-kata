package com.matthewk.domain

sealed trait DomainValidation {
  def message: String
}

case class SKUParseError(value: String) extends DomainValidation {
  val message = s"Unable to create an SKU with $value"
}

case class PriceParseError(value: BigDecimal) extends DomainValidation {
  val message = s"Prices must be greater than zero, you entered $value"
}

case class ItemNotFoundError(sku: SKU) extends DomainValidation {
  val message = s"Could not find an item matching sku: $sku"
}

case class QuantityError(quantity: Int) extends DomainValidation {
  val message = s"domain.Price rules must have a quantity > 1 you entered $quantity"
}

case object EmptyInputError extends DomainValidation {
  val message = "You did not provide any SKU codes"
}

case class UnknownInputError(cmd: String) extends DomainValidation {
  val message = s"I could not understand: $cmd"
}