package com.matthewk

import cats.data.Validated._
import cats.implicits._
import com.matthewk.domain.SKU._
import com.matthewk.domain._

/**
  * Here we are using applicative validators so that we do not fail fast when the input is incorrect.
  * Using Cats library, please note that I have had to enable partial unification in the compiler so that we don't
  * get type erasure when traversing a list SKUs to validate
  */

trait ItemValidatorNel extends InputValidation {

  override def validateSKU(sku: String): ValidationResult[SKU] =
    if (sku.matches("[A-Za-z]")) sku.toSKU.validNel else SKUParseError(sku).invalidNel

  private def validatePrice(price: BigDecimal): ValidationResult[Price] =
    if (price > 0) Price(price).validNel
    else PriceParseError(price).invalidNel

  private def validateQuantity(quantity: Int): ValidationResult[Int] =
    if (quantity > 1) quantity.validNel
    else QuantityError(quantity).invalidNel

  override def validateSkuExists(sku: SKU, catalogue: List[Item]): ValidationResult[Item] = catalogue.find(_.sku == sku) match {
    case Some(item) => item.valid
    case _          => ItemNotFoundError(sku).invalidNel
  }

  override def validateAddItems(input: List[String], catalogue: Catalogue): ValidationResult[List[Item]] =
    validateHasContent(input).andThen(
      skus =>
        skus.traverse[ValidationResult, Item](
          sku =>
            validateSKU(sku).andThen(s => validateSkuExists(s, catalogue.items.values.toList))
        )
    )

  override def validateHasContent(input: List[String]): ValidationResult[List[String]] =
    if (input.nonEmpty) input.validNel else EmptyInputError.invalidNel

  override def validateQuantityRule(sku: String, quantity: Int, price: BigDecimal, catalogue: List[Item]): ValidationResult[Rule] =
    (
      validateSKU(sku).andThen(s => validateSkuExists(s, catalogue)).asInstanceOf[ValidationResult[Item]],
      validateQuantity(quantity),
      validatePrice(price)
    ).mapN(QuantityRule)

}

object ItemValidatorNel extends ItemValidatorNel