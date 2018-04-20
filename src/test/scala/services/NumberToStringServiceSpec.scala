package services

import org.scalatest.{Matchers, WordSpec}

class NumberToStringServiceSpec extends WordSpec with Matchers {

  "numberToString" should {

    // Out of Range
    "return 'out of range' for -1" in {
      NumberToStringService.numberToString(-1) shouldBe "out of range"
    }
    "return 'out of range' for 10,000" in {
      NumberToStringService.numberToString(10000) shouldBe "out of range"
    }

    // 0-99
    "return 'zero' for 0" in {
      NumberToStringService.numberToString(0) shouldBe "zero"
    }
    "return 'one' for 1" in {
      NumberToStringService.numberToString(1) shouldBe "one"
    }
    "return 'nine' for 9" in {
      NumberToStringService.numberToString(9) shouldBe "nine"
    }
    "return 'fifteen' for 15" in {
      NumberToStringService.numberToString(15) shouldBe "fifteen"
    }
    "return 'twenty' for 20" in {
      NumberToStringService.numberToString(20) shouldBe "twenty"
    }
    "return 'twenty five' for 25" in {
      NumberToStringService.numberToString(25) shouldBe "twenty five"
    }
    "return 'seventy eight' for 78" in {
      NumberToStringService.numberToString(78) shouldBe "seventy eight"
    }

    // 100-999
    "return 'one hundred' for 100" in {
      NumberToStringService.numberToString(100) shouldBe "one hundred"
    }
    "return 'four hundred' for 400" in {
      NumberToStringService.numberToString(400) shouldBe "four hundred"
    }
    "return 'one hundred and one' for 101" in {
      NumberToStringService.numberToString(101) shouldBe "one hundred and one"
    }
    "return 'five hundred and fourteen' for 514" in {
      NumberToStringService.numberToString(514) shouldBe "five hundred and fourteen"
    }
    "return 'six hundred and thirty' for 630" in {
      NumberToStringService.numberToString(630) shouldBe "six hundred and thirty"
    }
    "return 'eight hundred and eighty two' for 882" in {
      NumberToStringService.numberToString(882) shouldBe "eight hundred and eighty two"
    }

    // 1000-9999
    "return 'one thousand' for 1000" in {
      NumberToStringService.numberToString(1000) shouldBe "one thousand"
    }
    "return 'two thousand' for 2000" in {
      NumberToStringService.numberToString(2000) shouldBe "two thousand"
    }
    "return 'three thousand and 8' for 3008" in {
      NumberToStringService.numberToString(3008) shouldBe "three thousand and eight"
    }
    "return 'four thousand and ninety four' for 4094" in {
      NumberToStringService.numberToString(4094) shouldBe "four thousand and ninety four"
    }
    "return 'five thousand three hundred' for 5300" in {
      NumberToStringService.numberToString(5300) shouldBe "five thousand three hundred"
    }
    "return 'six thousand seven hundred and fifty eight' for 6758" in {
      NumberToStringService.numberToString(6758) shouldBe "six thousand seven hundred and fifty eight"
    }
    "return 'seven thousand and eleven' for 7011" in {
      NumberToStringService.numberToString(7011) shouldBe "seven thousand and eleven"
    }
    "return 'eight thousand and forty' for 8040" in {
      NumberToStringService.numberToString(8040) shouldBe "eight thousand and forty"
    }
    "return 'nine thousand nine hundred and ninety nine' for 9999" in {
      NumberToStringService.numberToString(9999) shouldBe "nine thousand nine hundred and ninety nine"
    }
  }

}
