import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val originalPassword = lines.next()
    val password = originalPassword.toCharArray
    val M = lines.next().toInt
    val ops = lines.take(M).map(_.split(' ').map(_.toInt))

    for (op <- ops) {
      // type 1 command
      if (op(0) == 1) {
        val (i, j, k) = (op(1) - 1, op(2) - 1, op(3) - 1)
        val s1 = password.toString.substring(i, j + 1)
        val s2 = password.toString.substring(k, k + j - i + 1)
        if (s1.equals(s2)) {
          println("Y")
        } else {
          println("N")
        }
      }

      // type 2 command
      if (op(0) == 2) {
        val (i, j, k) = (op(1) - 1, op(2) - 1, op(3) - 1)
        for (pos <- i to j) {
          password(pos) = originalPassword.charAt(k + (pos - i))
        }
      }

      // type 3 command
      if (op(0) == 3) {
        val (i, j) = (op(1) - 1, op(2) - 1)
        for (pos <- i to j) {
          if (password(pos) == 'z') {
            password(pos) = 'a'
          } else {
            password(pos) = (password(pos) + 1).toChar
          }
        }
      }
    }
  }

}
