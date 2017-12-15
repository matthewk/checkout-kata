package com.matthewk

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.matthewk.domain.SKU._
import com.matthewk.domain._
import org.scalatest.{FunSpec, Matchers}

class ItemValidatorNelSpec extends FunSpec with Matchers {

  describe("ItemValidatorNelTest") {

    describe("validateQuantityRule") {
      it("should allow a Quantity Pricing Rule that has a quantity over Zero") {
        val item = Item("A".toSKU, Price(30))
        val catalogue = Catalogue(item)
        val validator = new ItemValidatorNel {}

        validator.validateQuantityRule("A", 1, BigDecimal(5), catalogue.items.values.toList) should be
        Valid(QuantityRule(item, 1, Price(5)))
      }

      it("should reject a Quantity Pricing Rule that has a quantity of Zero") {
        val item = Item("A".toSKU, Price(30))
        val catalogue = Catalogue(item)
        val validator = new ItemValidatorNel {}

        validator.validateQuantityRule("A", 0, BigDecimal(5), catalogue.items.values.toList) should be
        Invalid(NonEmptyList(QuantityError(0), List.empty))
      }
    }

    describe("validateSkuExists") {
      it("should allow an sku that can be found in the catalogue items") {
        val item = Item("A".toSKU, Price(30))
        val catalogue = Catalogue(item)
        val validator = new ItemValidatorNel {}

        validator.validateSkuExists("A".toSKU, catalogue.items.values.toList) should be
        Valid(item)
      }

      it("should reject an sku that cannot be found in the catalogue items") {
        val item = Item("A".toSKU, Price(30))
        val catalogue = Catalogue(item)
        val validator = new ItemValidatorNel {}

        validator.validateSkuExists("Z".toSKU, catalogue.items.values.toList) should be
        Invalid(NonEmptyList(ItemNotFoundError("Z".toSKU), List.empty))
      }

    }

    describe("validateHasContent") {
      it("should allow content that isn't empty") {
        val input = List("test")
        val validator = new ItemValidatorNel {}

        validator.validateHasContent(input) should be
        Valid(input)
      }

      it("should reject empty content") {
        val input = List.empty[String]
        val validator = new ItemValidatorNel {}

        validator.validateHasContent(input) should be
        Invalid(NonEmptyList(EmptyInputError, List.empty))
      }

    }

    describe("validateSKU") {
      it("should allow a single uppercase character between A and Z") {
        val input = "G"
        val validator = new ItemValidatorNel {}

        validator.validateSKU(input) should be
        Valid(input.toSKU)
      }

      it("should allow a single lowercase character between A and Z") {
        val input = "h"
        val validator = new ItemValidatorNel {}

        validator.validateSKU(input) should be
        Valid(input.toSKU)
      }

      it("should reject a single character that is not a letter A-Z") {
        val input = "!"
        val validator = new ItemValidatorNel {}

        validator.validateSKU(input) should be
        Invalid(NonEmptyList(SKUParseError(input), List.empty))
      }

      it("should reject multiple characters") {
        val input = "GA"
        val validator = new ItemValidatorNel {}

        validator.validateSKU(input) should be
        Invalid(NonEmptyList(SKUParseError(input), List.empty))
      }

    }

  }
}
