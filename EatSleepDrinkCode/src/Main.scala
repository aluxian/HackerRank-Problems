import scala.collection.mutable.ListBuffer
import scala.io.Source

case class TestCase(hours: Int, energy: Int, sleep: Int, drinkCount: Int, drink: Int, crash: Int)

case class Problem(energyRequired: Int, points: Int)

case class Alice(energyLevel: Int, drinksLeft: Int, points: Int = 0) {

  def canSolve(problem: Problem): Boolean = {
    problem.energyRequired <= energyLevel
  }

  def solve(problem: Problem): Alice = {
    copy(
      energyLevel = energyLevel - problem.energyRequired,
      points = points + problem.points
    )
  }

  def canDrinkCola(problem: Problem, boost: Int): Boolean = {
    drinksLeft > 0 && energyLevel + boost >= problem.energyRequired
  }

  def drinkCola(boost: Int): Alice = {
    copy(
      energyLevel = energyLevel + boost,
      drinksLeft = drinksLeft - 1
    )
  }

  def crashFromCaffeine(crash: Int): Alice = {
    copy(energyLevel = energyLevel - crash)
  }

  def sleep(boost: Int): Alice = {
    copy(energyLevel = energyLevel + boost)
  }

}

object Main {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val k = lines.next().toInt

    for (i <- 1 to k) {
      val (testCase, problems) = readTestCase(lines)
      val maxPoints = runTestCase(testCase, problems)
      println(maxPoints)
    }
  }

  def readTestCase(lines: Iterator[String]): (TestCase, List[Problem]) = {
    val values = lines.next().split(' ').map(_.toInt)
    val testCase = TestCase(values(0), values(1), values(2), values(3), values(4), values(5))
    val problems = lines
      .take(testCase.hours)
      .map(_.split(' ').map(_.toInt))
      .map(problemValues => Problem(problemValues(0), problemValues(1)))
      .toList
    (testCase, problems)
  }

  def runTestCase(testCase: TestCase, problems: List[Problem]): Int = {
    val game = new Game(testCase.hours, testCase.sleep, testCase.drink, testCase.crash, problems)
    val alice = Alice(testCase.energy, testCase.drinkCount)
    game.run(alice)
  }

}

class Game(hours: Int, sleepBoost: Int, drinkBoost: Int, drinkCrash: Int, problems: List[Problem]) {

  // define a type for the decision functions
  type DecisionFn = (Alice, Problem) => Option[(Alice, Boolean)]

  // list with the hours when the caffeine crash effect will be applied
  val caffeineCrashHours = ListBuffer[Int]()

  // possible decisions
  val decisions = List[DecisionFn](
    // attempt to solve the problem
    (alice, problem) => {
      if (alice.canSolve(problem)) {
        Some(alice.solve(problem), false)
      } else {
        None
      }
    },

    // skip the problem and sleep
    (alice, problem) => {
      Some(alice.sleep(sleepBoost), false)
    },

    // drink cola and attempt the problem
    (alice, problem) => {
      if (alice.canDrinkCola(problem, drinkBoost)) {
        Some(alice.drinkCola(drinkBoost).solve(problem), true)
      } else {
        None
      }
    }
  )

  def run(alice: Alice): Int = {
    val (newAlice, logs) = run(alice, 0)
    logs.get.foreach(println)
    newAlice.points
  }

  def run(alice: Alice, hour: Int): (Alice, Option[ListBuffer[String]]) = {
    println(s"running alice hour: $hour")
    val logBuffer = ListBuffer[String]()
    val problem = problems(hour)
    val alices = decisions.zipWithIndex.map[(Alice, Option[ListBuffer[String]]), List[(Alice, Option[ListBuffer[String]])]](d => {
      val (decision, index) = d
      // try to take the decision
      val result = decision(alice, problem)
      if (result.isDefined) {
        // save when the caffeine effect will occur again
        val (aliceAfterDecision, hasCaffeineEffect) = result.get
        if (hasCaffeineEffect) {
          caffeineCrashHours += hour + 2
        }

        // ensure there is a next hour
        val nextHour = hour + 1
        if (nextHour < hours) {
          // caffeine crash effect for the next hour
          val aliceAfterCrash = if (caffeineCrashHours.contains(nextHour)) {
            aliceAfterDecision.crashFromCaffeine(drinkCrash)
          } else {
            aliceAfterDecision
          }

          val crashLog = if (hasCaffeineEffect) {
            "affectedByCaffeine"
          } else {
            "notAffectedByCaff"
          }

          logBuffer += s"hour $hour running decision $index success $aliceAfterCrash $crashLog"
          run(aliceAfterCrash, nextHour)
        } else {
          logBuffer += s"hour $hour running decision $index success (last hour) $alice"
          (alice, None)
        }
      } else {
        logBuffer += s"hour $hour running decision $index failed $alice"
        (alice, None)
      }
    })

    val chosenAlice = alices.maxBy(_._1.points)
    logBuffer += s"hour $hour choosing decision ${alices.indexOf(chosenAlice)}"
//    logBuffer += chosenAlice._1.toString
    logBuffer += ""
    chosenAlice.copy(_2 = Some(logBuffer ++ chosenAlice._2.getOrElse(ListBuffer[String]())))
  }

}
