package com.matthewk.domain

sealed abstract class SKU(value: Char){
  override def toString: String = value.toString
}

object SKU {
  implicit class StringOps(s: String) extends StringSKUConverter(s)
}

case object InvalidSKU extends SKU('?')

case class ValidSKU private(value: Char) extends SKU(value)

class StringSKUConverter(skuString: String) {

  def toSKU: SKU = validate(skuString).toOption.getOrElse(InvalidSKU)

  private def validate(unChecked: String): Either[String, SKU] =
    if (unChecked.matches("[A-Z]")) unChecked.headOption.map(ValidSKU).toRight(unChecked) else Left(unChecked)
}
