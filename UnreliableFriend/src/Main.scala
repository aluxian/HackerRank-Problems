import Color.Color

import scala.io.Source

object Color extends Enumeration {

  type Color = Value

  val Red = Value("r")
  val Green = Value("g")
  val Blue = Value("b")

}

case class TestCase(numLies: Int, questions: List[Question])

case class Question(statement: Statement, isTrue: Boolean)

sealed trait Statement

case class AndStatement(children: Statement*) extends Statement

case class OrStatement(children: Statement*) extends Statement

case class ColorStatement(index: Int, color: Color) extends Statement

case class CountStatement(color: Color, count: Int) extends Statement

case class Balloon(id: Int, colors: List[Color])

object UnreliableFriend {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines
    val t = lines.next().toInt

    for (i <- 1 to t) {
      val testCase = readTestCase(lines)
      val colors = runTestCase(testCase)
      println(colors.mkString(" "))
    }
  }

  def readTestCase(lines: Iterator[String]): TestCase = {
    lines.next() // skip the blank line
    val qnValues = lines.next().split(' ').map(_.toInt)
    val q = qnValues(0)
    val n = qnValues(1)
    val questions = 1.to(q).toList.map(i => {
      // println(s"reading q $i")
      val spec = parseStatement(lines.next())
      val answer = parseAnswer(lines.next())
      Question(spec, answer)
    })
    TestCase(n, questions)
  }

  def parseStatement(line: String): Statement = {
    // println(s"parsing line: $line")

    if (line.contains(" and ")) {
      return AndStatement(line.split(" and ").map(parseStatement).toList: _*)
    }

    if (line.contains(" or ")) {
      return OrStatement(line.split(" or ").map(parseStatement).toList: _*)
    }

    if (line.startsWith("color")) {
      val parts = line.split(' ')
      val index = parts(1).toInt
      val color = Color.withName(parts(2))
      return ColorStatement(index, color)
    }

    if (line.startsWith("count")) {
      val parts = line.split(' ')
      val color = Color.withName(parts(1))
      val count = parts(2).toInt
      return CountStatement(color, count)
    }

    throw new RuntimeException(s"Cannot parse line: $line")
  }

  def parseAnswer(answer: String): Boolean = {
    answer match {
      case "yes" => true
      case "no" => false
      case _ => throw new RuntimeException(s"Cannot parse answer: '$answer'")
    }
  }

  def runTestCase(testCase: TestCase): List[String] = {
    val orderedColors = List(Color.Red, Color.Green, Color.Blue)
    val balloons = 1.to(10).map(i => Balloon(i, orderedColors)).toList
    balloonsToColors(identify(balloons, testCase.numLies, testCase.questions))
  }

  def balloonsToColors(balloons: List[Balloon]): List[String] = {
    balloons.map(_.colors.mkString(""))
  }

  def identify(balloons: List[Balloon], numLies: Int, questions: List[Question]): List[Balloon] = {
    if (questions.isEmpty) {
      return balloons
    }

    val question = questions.head

    if (numLies == 0) {
      // all the remaining questions are definitely true
      val identifiedBalloons = evaluateQuestion(balloons, question)
      return identify(identifiedBalloons, numLies - 1, questions.tail)
    }

    if (questions.size == numLies // all the remaining questions are lies
      || isObviouslyFalse(question) // we found 1 lie
    ) {
      val correctedQuestion = question.copy(isTrue = !question.isTrue)
      val identifiedBalloons = evaluateQuestion(balloons, correctedQuestion)
      return identify(identifiedBalloons, numLies - 1, questions.tail)
    }

    // we have some lies and some questions, but we don't know which is which
    // assume this question is true

    val identifiedBalloons = evaluateQuestion(balloons, question)
    identify(identifiedBalloons, numLies - 1, questions.tail)
  }

  def isObviouslyFalse(question: Question): Boolean = {
    question.statement match {
      case andStatement: AndStatement =>
        question.isTrue &&
          andStatement.children.forall(_.isInstanceOf[CountStatement]) &&
          andStatement.children.map(_.asInstanceOf[CountStatement].count).sum <= 10
      case orStatement: OrStatement =>
        !question.isTrue &&
          orStatement.children.forall(_.isInstanceOf[CountStatement]) &&
          orStatement.children.map(_.asInstanceOf[CountStatement].count).sum <= 10
      case _ => false
    }
  }

  var counts = Map[Color, Int]()

  def evaluateQuestion(balloons: List[Balloon], question: Question): List[Balloon] = {
    question.statement match {
      case andStatement: AndStatement =>
        if (question.isTrue && andStatement.children.forall(_.isInstanceOf[ColorStatement])) {
          var newBalloons = balloons
          andStatement.children.map(_.asInstanceOf[ColorStatement]).foreach(cs => {
            newBalloons = evaluateQuestion(newBalloons, Question(cs, isTrue = true))
          })
          newBalloons
        } else {
          balloons
        }

      case orStatement: OrStatement =>
        if (!question.isTrue && orStatement.children.forall(_.isInstanceOf[ColorStatement])) {
          // inverse and
          var newBalloons = balloons
          orStatement.children.map(_.asInstanceOf[ColorStatement]).foreach(cs => {
            newBalloons = evaluateQuestion(newBalloons, Question(cs, isTrue = false))
          })
          newBalloons
        } else {
          // if all are false except one, that exception must be true
          allExceptOneAreFalse(balloons, orStatement.children)
            .map(trueStatementException => {
              evaluateQuestion(balloons, Question(trueStatementException, isTrue = true))
            })
            .getOrElse(balloons)
          //            .getOrElse(() => {
          //              //              // check count
          //              //              if (counts.keySet
          //              //              balloons.count(_.colors.contains(color))
          //
          //            })
        }

      case ColorStatement(index, color) =>
        val balloon = balloons.find(_.id == index).get
        val newBalloon = if (question.isTrue) {
          balloon.copy(colors = List(color))
        } else {
          balloon.copy(colors = balloon.colors.diff(Seq(color)))
        }
        balloons.updated(balloons.indexOf(balloon), newBalloon)

      case CountStatement(color, count) =>
        counts = counts.updated(color, count)
        // TODO
        balloons
    }
  }

  def isStatementTrue(balloons: List[Balloon])(statement: Statement): Boolean = {
    statement match {
      case andStatement: AndStatement => andStatement.children.forall(isStatementTrue(balloons))
      case orStatement: OrStatement => orStatement.children.exists(isStatementTrue(balloons))
      case ColorStatement(index, color) => balloons.find(_.id == index).get.colors.contains(color)
      case CountStatement(color, count) => balloons.count(_.colors.contains(color)) == count
    }
  }

  def allExceptOneAreFalse(balloons: List[Balloon], statements: Seq[Statement]): Option[Statement] = {
    for (statement <- statements) {
      if (statements.diff(Seq(statement)).forall(s => !isStatementTrue(balloons)(s))) {
        return Some(statement)
      }
    }

    None
  }

}
