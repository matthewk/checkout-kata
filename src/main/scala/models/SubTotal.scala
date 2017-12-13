package models

case class SubTotal(remainingItems: Basket, price: Price = Price.Zero) {
  val asTotal: Price = price + remainingItems.total

  override def toString: String = s"$remainingItems\nSubTotal: $price"
}
