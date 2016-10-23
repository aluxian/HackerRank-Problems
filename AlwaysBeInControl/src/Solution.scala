import scala.io.Source

case class Group(data: List[Int], average: Double, range: Double)

object Solution {

  val A2 = Map(
    2 -> 1.880,
    3 -> 1.023,
    4 -> 0.729,
    5 -> 0.577,
    6 -> 0.483,
    7 -> 0.419,
    8 -> 0.373,
    9 -> 0.337,
    10 -> 0.308
  )

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val t = lines.next().toInt
    lines.take(t).foreach(line => {
      val specs = line.split(' ').map(_.toInt)
      val groupSize = specs(1)
      val entries = specs.drop(2).toList
      if (analyse(groupSize, entries)) {
        println("In Control")
      } else {
        println("Out of Control")
      }
    })
  }

  def analyse(groupSize: Int, entries: List[Int]): Boolean = {
    val groups = entries
      .grouped(groupSize)
      .toList
      .map(items => {
        val avg = items.sum.toDouble / items.size.toDouble
        val range = items.max - items.min
        Group(items, avg, range)
      })

    val Xave = groups.map(_.average).sum / groups.size.toDouble
    val Rave = groups.map(_.range).sum / groups.size.toDouble

    val UCL = Xave + A2(groupSize) * Rave
    val LCL = Xave - A2(groupSize) * Rave
    val CL = Xave
    val sigma = (UCL - CL) / 3

    // println("groups:" + groups.mkString("\n"))
    // println(s"Xave=$Xave")
    // println(s"Rave=$Rave")
    // println(s"UCL=$UCL")
    // println(s"LCL=$LCL")
    // println(s"CL=$CL")
    // println(s"sigma=$sigma")

    def sameSide(points: Int*): Boolean = {
      if (points.size <= 1) {
        return true
      }

      val head = points.head
      val aboveBelow = head > CL

      points.tail.forall(p => aboveBelow == (p > CL))
    }

    def distanceFromCenter(point: Int): Double = {
      Math.abs(point.toDouble - CL)
    }

    def isSigmaAway(timesSigma: Int, points: Int*): Boolean = {
      points.forall(p => distanceFromCenter(p) > timesSigma * sigma)
    }

    def control1Failed(): Boolean = {
      entries.exists(v => v > UCL || v < LCL)
    }

    def control2Failed(): Boolean = {
      entries.sliding(3).exists(window => {
        window.combinations(2)
          .exists(pair => sameSide(pair: _*) && isSigmaAway(2, pair: _*))
      })
    }

    def control3Failed(): Boolean = {
      entries.sliding(5).exists(window => {
        window.combinations(4)
          .exists(pair => sameSide(pair: _*) && isSigmaAway(1, pair: _*))
      })
    }

    def control4Failed(): Boolean = {
      entries.sliding(8).exists(sameSide)
    }

    !(control1Failed() || control2Failed() || control3Failed() || control4Failed())
  }

}
