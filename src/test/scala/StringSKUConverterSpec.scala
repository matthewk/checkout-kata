import com.matthewk.domain._
import org.scalatest.{FunSpec, Matchers}

class StringSKUConverterSpec extends FunSpec with Matchers {

  describe("StringSKUConverter") {

    describe("asEitherSKU") {
      it("should fail to extract a non letter string to a SKU") {
        val input: String = "1"
        val stringSKUConverter = new StringSKUConverter(input)
        stringSKUConverter.toSKU match {
          case ValidSKU(_) => fail(s"Should only extract letters from $input")
          case InvalidSKU  => succeed
        }
      }

      it("should extract a single letter string to a SKU") {
        val input: String = "A"
        val stringSKUConverter = new StringSKUConverter(input)
        stringSKUConverter.toSKU match {
          case ValidSKU(c) => c should be('A')
          case InvalidSKU  => fail(s"Could not extract an SKU from $input")
        }
      }

      it("should fail to extract a series of letters to a SKU") {
        val input: String = "HELLO"
        val stringSKUConverter = new StringSKUConverter(input)
        stringSKUConverter.toSKU match {
          case ValidSKU(_) => fail(s"should only extract single letters from $input")
          case InvalidSKU  => stringSKUConverter.toSKU.toString should be("?")
        }
      }

      it("should fail) when given no characters") {
        val input: String = ""
        val stringSKUConverter = new StringSKUConverter(input)
        stringSKUConverter.toSKU match {
          case ValidSKU(_) => fail(s"could not extract an SKU from $input")
          case InvalidSKU  => succeed
        }
      }
    }


    describe("toSKU") {
      it("should fail to extract a non letter string to a SKU") {
        val input: String = "1"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be(InvalidSKU)
      }

      it("should extract a single letter string to a SKU") {
        val input: String = "A"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be(ValidSKU('A'))
      }

      it("should fail to extract a series of letters to a SKU") {
        val input: String = "HELLO"
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be(InvalidSKU)
      }

      it("should return a Left(String) when given no characters") {
        val input: String = ""
        val stringSKUConverter = new StringSKUConverter(input) {}
        stringSKUConverter.toSKU should be(InvalidSKU)
      }
    }

  }
}
