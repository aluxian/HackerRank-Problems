

import scala.io.Source

case class TestCase(numPages: Int, pageSize: Int, addresses: List[Int])

abstract class Cache(size: Int) {

  var numReplacements = 0

  def access(index: Int): Unit

  def getNumReplacements: Int = numReplacements

}

class FIFO(size: Int) extends Cache(size) {

  val memory = new Array[Int](size).map(_ => -1)
  var memoryPointer = 0

  override def access(index: Int): Unit = {
    if (memory.contains(index)) {
      return
    }


    if (memory(memoryPointer) > -1) {
      numReplacements += 1
    }
    memory(memoryPointer) = index
    memoryPointer = (memoryPointer + 1) % size
  }

}

class LRU(size: Int) extends Cache(size) {

  var memory = List[Int]()

  override def access(index: Int): Unit = {
    if (memory.contains(index)) {
      memory = memory.diff(Seq(index)) :+ index
      return
    }

    if (memory.size >= size) {
      memory = memory.tail :+ index
      numReplacements += 1
    } else {
      memory = memory :+ index
    }
  }

}

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val t = lines.next().toInt

    for (i <- 1 to t) {
      val testCase = readTestCase(lines)
      val replacements = runTestCase(testCase)
      val ad = if (replacements._1 > replacements._2) "yes" else "no"
      println(s"$ad ${replacements._1} ${replacements._2}")
    }
  }

  def readTestCase(lines: Iterator[String]): TestCase = {
    val psn = lines.next().split(' ').map(_.toInt)
    val (p, s, n) = (psn(0), psn(1), psn(2))
    val addresses = lines.take(n).map(address => Math.floor(address.toDouble / s).toInt).toList
    TestCase(p, s, addresses)
  }

  def runTestCase(testCase: TestCase): (Int, Int) = {
    val fifoCache = new FIFO(testCase.numPages)
    testCase.addresses.foreach(fifoCache.access)

    val lruCache = new LRU(testCase.numPages)
    testCase.addresses.foreach(lruCache.access)

    (fifoCache.getNumReplacements, lruCache.getNumReplacements)
  }

}
