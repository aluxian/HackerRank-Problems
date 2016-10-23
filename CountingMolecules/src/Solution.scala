import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val atoms = lines.next().split(' ').map(_.toLong)

    var c: Long = atoms(0)
    var h: Long = atoms(1)
    var o: Long = atoms(2)

    var h2o: Long = 0
    var co2: Long = 0
    var c6h12o6: Long = 0

    while (c >= 6 && h >= 12 && o >= 6) {
      c6h12o6 += 1
      c -= 6
      h -= 12
      o -= 6
    }

    while (h >= 2 && o >= 1) {
      h2o += 1
      h -= 2
      o -= 1
    }

    while (c >= 1 && o >= 2) {
      co2 += 1
      c -= 1
      o -= 2
    }

    if (c <= 0 && h <= 0 && o <= 0) {
      println(s"$h2o $co2 $c6h12o6")
    } else {
      println(s"Error")
    }
  }

}
