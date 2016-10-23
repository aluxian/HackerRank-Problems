import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Bob(brushChanges: Int = 0) {
  def changeBrush(): Bob = {
    copy(brushChanges = brushChanges + 1)
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val t = lines.next().toInt

    for (i <- 1 to t) {
      lines.next() // skip number
      val colors = lines.next().split(' ').map(_.toInt).toList
      val (bob, logs) = run(Bob(), colors, (-1, -1))
      logs.foreach(println)
      println(bob.brushChanges)
    }
  }

  var  count = 0
  val cache = mutable.Map[(List[Int], (Int, Int)), (Bob, ListBuffer[String])]()

  def run(bob: Bob, colors: List[Int], brushes: (Int, Int)): (Bob, ListBuffer[String]) = {
    val logs = ListBuffer[String]()
//    logs += s"run() bob: $bob"

    if (colors.isEmpty) {
//      logs += "ran out of colors"
      return (bob, logs)
    }

    val currentColor = colors.head
    val restOfColors = colors.tail
//    logs += s"run() color: $currentColor, brushes: $brushes"
    println(s"run() color: $currentColor, brushes: $brushes, left: ${colors.size}")
    count += 1
    if (count >= 500) {
      throw new RuntimeException("sdafa")
    }

//    if (brushes._1 == currentColor || brushes._2 == currentColor) {
//      logs += "color already present on brush, moving to next\n"
//      val (_bob, _logs) = run(bob, restOfColors, brushes)
//      return (_bob, logs ++ _logs)
//    }

    val newBrushes1 = (currentColor, brushes._2)
    val newBrushes2 = (brushes._1, currentColor)

    val bob1 = if (brushes._1 != currentColor) bob.changeBrush() else bob
    val bob2 = if (brushes._2 != currentColor) bob.changeBrush() else bob

//    logs += s"run() bob1: $bob1 bob2: $bob2"
//    logs += s"run() new brushes1: $newBrushes1 new brushes2: $newBrushes2"

//    logs += "run() making decisions"
    val (newBob1, log1) = if (cache.contains((restOfColors, newBrushes1))) {
      cache((restOfColors, newBrushes1))
    } else {
      val x = run(bob1, restOfColors, newBrushes1)
      cache((restOfColors, newBrushes1)) = x
      x
    }

    val (newBob2, log2) = if (cache.contains((restOfColors, newBrushes2))) {
      cache((restOfColors, newBrushes2))
    } else {
      val x = run(bob2, restOfColors, newBrushes2)
      cache((restOfColors, newBrushes2)) = x
      x
    }

//    logs += s"run() newBob1: $newBob1 newBob2: $newBob2"

    if (newBob1.brushChanges <= newBob2.brushChanges) {
//      logs += "run() chose left brush\n"
      (newBob1, logs ++ log1)
    } else {
//      logs += "run() chose right brush\n"
      (newBob2, logs ++ log2)
    }
  }

}
