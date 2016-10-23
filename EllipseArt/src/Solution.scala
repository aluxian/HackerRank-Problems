import scala.io.Source

case class Ellipsis(x1: Int, y1: Int, x2: Int, y2: Int, r: Int) {
  def getArea: Double = {
    import Math._
    r / 4 * PI * sqrt(pow(r, 2) - pow(x2 - x1, 2))
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val t = lines.next().toInt

    for (i <- 1 to t) {
      val numEllipses = lines.next().toInt
      val ellipses = lines.take(numEllipses)
        .map(_.split(' ').map(_.toInt))
        .map(p => Ellipsis(p(0), p(1), p(2), p(3), p(4)))
        .toList
      val area = calculateArea(ellipses)
      println(area + "%")
    }
  }

  def calculateArea(ellipses: List[Ellipsis]): Int = {
    val totalCanvasArea = 100 * 100
    val totalPaintedArea = sumUpEllipseAreas(ellipses)
    val percentage = (totalCanvasArea - totalPaintedArea) / totalCanvasArea
    Math.round(percentage * 100).toInt
  }

  def sumUpEllipseAreas(ellipses: List[Ellipsis]): Double = {
    var sum = ellipses.map(_.getArea).sum
    // subtract areas that fall outside the canvas
    // subtract areas that overlap
    sum
  }

}
