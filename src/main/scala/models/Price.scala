package models

case class Price(value: BigDecimal){
  private final val currency: String = "£"

  def +(that: Price): Price = copy(this.value + that.value)

  override def toString: String = s"$currency$value"
}

object Price {
  object Zero extends Price(0)
}

