package models

import org.scalatest.{FunSpec, Matchers}

class StringSKUConverterSpec extends FunSpec with Matchers{

  describe("StringSKUConverter") {

    describe("asEitherSKU") {
      it("should fail to extract a non letter string to a SKU") {
        val input: String = "1"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.asEitherSKU.fold(_ should be ("1"), _ => fail(s"Should only extract letters from $input"))
      }

      it("should extract a single letter string to a SKU") {
        val input: String = "A"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.asEitherSKU.fold(_ => fail(s"Could not extract an SKU from $input"), _ should be (ValidSKU('A')))
      }

      it("should fail to extract a series of letters to a SKU") {
        val input: String = "HELLO"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.asEitherSKU.fold(_ should be ("HELLO"), _ => fail(s"should only extract single letters from $input"))
      }

      it("should return a Left(String) when given no characters") {
        val input: String = ""
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.asEitherSKU.fold(_ should be (""), _ => fail(s"could not extract an SKU from $input"))
      }
    }


    describe("toSKU") {
      it("should fail to extract a non letter string to a SKU") {
        val input: String = "1"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be (InvalidSKU)
      }

      it("should extract a single letter string to a SKU") {
        val input: String = "A"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be (ValidSKU('A'))
      }

      it("should fail to extract a series of letters to a SKU") {
        val input: String = "HELLO"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be (InvalidSKU)
      }

      it("should return a Left(String) when given no characters") {
        val input: String = ""
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be (InvalidSKU)
      }
    }

  }
}
