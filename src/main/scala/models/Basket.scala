package models

case class Basket private(items: List[Item]){

  def addItem(item: Item): Basket = copy(items = items :+ item)

  def addItemQuantity(quantity: Int, item: Item): Basket = copy(items = items ++ (1 to quantity).map(_ => item))

  def addItems(newItems: List[(Int, Item)]): Basket = copy(items = newItems.flatMap(i => addItemQuantity(i._1, i._2).items))

  lazy val total: Price = (items.map(_.price) :+ Price.Zero).reduce(_ + _)

  override def toString: String = items.map(_.toString).mkString("\n")

}

object Basket {
  def apply(items: Item*): Basket = new Basket(items.toList)

  object Empty extends Basket(List.empty[Item])
}



