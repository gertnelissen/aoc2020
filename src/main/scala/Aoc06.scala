import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc06 {
  def countUniqueAnswers(input: List[String]): Int = {
    val solver = new Aoc06()
    val answersPerGroup = solver.parse(input)
    solver.countUniqueAnswers(answersPerGroup)
  }

  def countCommonAnswers(input: List[String]): Int = {
    val solver = new Aoc06()
    val answersPerGroup = solver.parse(input)
    solver.countCommonAnswers(answersPerGroup)
  }
}

class Aoc06() {
  def parse(input: List[String]): Array[Array[Array[Char]]] = {
    input.mkString("-").split("--").map(parseAnswers)
  }

  def parseAnswers(input: String): Array[Array[Char]] = {
    input.split(Array('\n', '-')).map(_.toCharArray)
  }

  def countUniqueAnswers(answerGroups: Array[Array[Array[Char]]]): Int = {
    answerGroups.map(countUniqueAnswers).sum
  }

  def countUniqueAnswers(answers: Array[Array[Char]]): Int = {
    answers.flatten.toSet.size
  }

  def countCommonAnswers(answerGroups: Array[Array[Array[Char]]]): Int = {
    answerGroups.map(countCommonAnswers).sum
  }

  def countCommonAnswers(answers: Array[Array[Char]]): Int = {
    answers.flatten.groupBy(answers => answers).count(answerGroup => answerGroup._2.length == answers.length)
  }
}

class Aoc06Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """abc
        |
        |a
        |b
        |c
        |
        |ab
        |ac
        |
        |a
        |a
        |a
        |a
        |
        |b""".stripMargin)

    Aoc06.countUniqueAnswers(input) shouldBe 11
  }

  test("Puzzle 1") {
    Aoc06.countUniqueAnswers(readStrings("Aoc06.txt")) shouldBe 6763
  }

  test("Ex 2") {
    val input = textToStrings(
      """abc
        |
        |a
        |b
        |c
        |
        |ab
        |ac
        |
        |a
        |a
        |a
        |a
        |
        |b""".stripMargin)

    Aoc06.countCommonAnswers(input) shouldBe 6
  }

  test("Puzzle 2") {
    Aoc06.countCommonAnswers(readStrings("Aoc06.txt")) shouldBe 3512
  }
}