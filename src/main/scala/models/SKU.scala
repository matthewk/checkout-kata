package models

sealed abstract class SKU(value: Char){
  override def toString: String = value.toString
}

case class ValidSKU private(value: Char) extends SKU(value)
case object InvalidSKU extends SKU('?')

object SKU {
  implicit class StringOps(s: String) extends StringSKUConverter(s)

  def fromString(s: String): SKU = s.toSKU
}

abstract class StringSKUConverter(s: String) {

  def toSKU: SKU = validate(s).toOption.getOrElse(InvalidSKU)

  def asEitherSKU: Either[String, SKU] = validate(s)

  private def validate(unChecked: String): Either[String, SKU] = if (unChecked.matches("[A-Z]")) {
    unChecked.headOption.map(ValidSKU).toRight(unChecked)
  } else {
    Left(unChecked)
  }

}


