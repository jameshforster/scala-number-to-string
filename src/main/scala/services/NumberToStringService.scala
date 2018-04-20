package services

object NumberToStringService {

  def numberToString(num: Int): String = {
    if (num >= 0 && num < 10000) {
      thousandsToString(num)
    } else "out of range"
  }

  def unitToString(num: Int): String = {
    val map: Map[Int, String] = Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine"
    )

    if (num == 0) "zero"
    else map.getOrElse(num, "")
  }

  def nonTeenValueToString(tenPart: Int, unitPart: Int) = {
    val map: Map[Int, String] = Map(
      2 -> "twenty",
      3 -> "thirty",
      4 -> "forty",
      5 -> "fifty",
      6 -> "sixty",
      7 -> "seventy",
      8 -> "eighty",
      9 -> "ninety"
    )

    map.getOrElse(tenPart.toInt, "") + {
      if (unitPart != 0) " " + unitToString(unitPart) else ""
    }
  }

  def tensToString(hundredRemainder: BigDecimal) = {
    val tenPart = hundredRemainder/10
    val unitPart = hundredRemainder.remainder(10)

    val map: Map[Int, String] = Map(
      10 -> "ten",
      11 -> "eleven",
      12 -> "twelve",
      13 -> "thirteen",
      14 -> "fourteen",
      15 -> "fifteen",
      16 -> "sixteen",
      17 -> "seventeen",
      18 -> "eighteen",
      19 -> "nineteen")

    if (hundredRemainder < 10) unitToString(hundredRemainder.toInt)
    else map.getOrElse(hundredRemainder.toInt, nonTeenValueToString(tenPart.toInt, unitPart.toInt))
  }

  def hundredsToString(thousandRemainder: BigDecimal) = {
    val hundredPart = thousandRemainder/100
    val hundredRemainder = thousandRemainder.remainder(100)

    if (hundredPart >= 1 && hundredRemainder == 0) unitToString(hundredPart.toInt) + " hundred"
    else if (hundredPart >= 1) unitToString(hundredPart.toInt) + " hundred and " + tensToString(hundredRemainder.toInt)
    else tensToString(hundredRemainder.toInt)
  }

  def thousandsToString(num: BigDecimal) = {
    val thousandPart = num/1000
    val thousandRemainder = num.remainder(1000)

    if (thousandPart >= 1 && thousandRemainder == 0) unitToString(thousandPart.toInt) + " thousand"
    else if (thousandPart >= 1 && thousandRemainder > 99) unitToString(thousandPart.toInt) + " thousand " + hundredsToString(thousandRemainder)
    else if (thousandPart >= 1) unitToString(thousandPart.toInt) + " thousand and " + hundredsToString(thousandRemainder)
    else hundredsToString(thousandRemainder)
  }
}
