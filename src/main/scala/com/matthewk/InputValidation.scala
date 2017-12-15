package com.matthewk

import cats.data.ValidatedNel
import com.matthewk.domain._

trait InputValidation {

  type ValidationResult[A] = ValidatedNel[DomainValidation, A]
  type Items = List[Item]

  def validateSKU(sku: String): ValidationResult[SKU]
  def validateSkuExists(sku: SKU, catalogue: List[Item]): ValidationResult[Item]
  def validateAddItems(input: List[String], catalogue: Catalogue): ValidationResult[List[Item]]
  def validateHasContent(input: List[String]): ValidationResult[List[String]]
  def validateQuantityRule(sku: String, quantity: Int, price: BigDecimal, catalogue: List[Item]): ValidationResult[Rule]
}
