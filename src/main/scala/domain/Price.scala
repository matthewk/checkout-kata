package domain

case class Price(amount: BigDecimal) {
  private final val currency: String = "£"

  def +(that: Price): Price = copy(this.amount + that.amount)

  def -(that: Price): Price = copy(this.amount - that.amount)

  override def toString: String = s"$currency$amount"
}

object Price {

  object Zero extends Price(0)

}
