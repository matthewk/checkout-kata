package com.matthewk.domain

/**
  * The collection of items that are available le in the store
  *
  * @note We are using a smart constructor here to manage the conversion of the List of items into a KV Store
  * @param items The store of items keyed on their SKU
  */
case class Catalogue private(items: Map[SKU, Item])

object Catalogue {
  def apply(items: Item*): Catalogue = new Catalogue(items.toList.map(i => (i.sku, i)).toMap)

  object Empty extends Catalogue(Map.empty[SKU, Item])
}

case class Item(sku: SKU, price: Price) {
  override def toString: String = s"Item - ${sku.toString} unit price: ($price)"
}