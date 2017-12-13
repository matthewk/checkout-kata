package models

case class Item(sku: SKU, price: Price) {
  override def toString: String = s"Item - ${sku.toString} with price: $price"
}
