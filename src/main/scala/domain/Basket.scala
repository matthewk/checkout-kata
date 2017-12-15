package domain

case class Basket private(items: List[Item]) {

  def addItem(item: Item): Basket = copy(items = items :+ item)

  def addItems(newItems: List[Item]): Basket = copy(items = newItems.flatMap(i => addItem(i).items))

  lazy val total: Price = (items.map(_.price) :+ Price.Zero).reduce(_ + _)

  def renderWithSavings(rules: PricingRules): String = {
    items.groupBy(_.sku).map {
      case (_, v) =>
        val cost = rules.getSavingForItem(v.head, v.size)
        val saving = v.map(_.price).reduce(_ + _) - cost.asTotal
        val maybeSaving = if (saving.amount > 0) s" *** SAVING: ${saving.toString} ***" else ""
        s"${v.size} x ${v.head} -> ${cost.asTotal} $maybeSaving"
    }.mkString("\n")
  }

  override def toString: String =
    items.groupBy(_.sku).map {
      case (_, v) => s"${v.size} x ${v.head} (${v.map(_.price).reduce(_ + _)})  "
    }.mkString("\n")
}

object Basket {
  def apply(items: Item*): Basket = new Basket(items.toList)

  object Empty extends Basket(List.empty[Item])
}