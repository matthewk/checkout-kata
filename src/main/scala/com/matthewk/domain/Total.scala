package com.matthewk.domain

case class Total(cost: Price, fullCost: Price) {
  private lazy val saving = fullCost - cost

  val hasSaving: Boolean = cost.amount < fullCost.amount

  private def maybeSavingToString = if (hasSaving) {
    s"** Saving: $saving **"
  } else ""

  override def toString: String = s"Total: $cost \t$maybeSavingToString"
}


case class SubTotal(remainingItems: Basket, price: Price = Price.Zero) {
  val asTotal: Price = price + remainingItems.totalPrice

  override def toString: String = s"$remainingItems\nSubTotal: $price"
}
