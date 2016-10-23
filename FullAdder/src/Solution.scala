import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val input = lines.take(5).toArray

    val input1 = input(0).split(' ')
    // val b = input1(0).toInt
    val symbols = input1(1).toCharArray

    val t1 = input(1).trim.toCharArray
    val t2 = input(2).tail.trim.toCharArray

    // println(s"t1: ${t1.toList}")
    // println(s"t2: ${t2.toList}")

    val result = run(symbols, t1, t2).mkString
    val lengthDiff = input(3).length - result.length
    val prefix = Array.fill(lengthDiff)(' ').mkString
    val output = input.take(4) ++ Array[String](prefix + result)

    // println(s"dec IZvOS67vvZe88vcZ to dec: ${decode(symbols, "IZvOS67vvZe88vcZ".toCharArray)}")

    println(output.mkString("\n"))
  }

  def run(symbols: Array[Char], t1: Array[Char], t2: Array[Char]): Array[Char] = {
    // val decimalSum = 1940961558751899592L//decode(symbols, t1) + decode(symbols, t2)
    val decimalSum = decode(symbols, t1) + decode(symbols, t2)
    // println(s"decimal sum = $decimalSum")
    encode(symbols, decimalSum)
  }

  def encode(symbols: Array[Char], number: Long): Array[Char] = {
    var num: Double = number

    if (num == 0) {
      return Array(symbols.head)
    }

    val encodedStr = new StringBuilder()
    while (num > 0) {
      encodedStr.insert(0, symbols((num % symbols.length).toInt))
      num = Math.floor(num / symbols.length.toDouble)
    }

    encodedStr.toArray
  }

  def decode(symbols: Array[Char], encodedChars: Array[Char]): Long = {
    encodedChars.reverse.zipWithIndex.map({
      case (character, index) =>
        symbols.indexOf(character) * Math.pow(symbols.length, index)
    }).sum.toLong
  }

}
