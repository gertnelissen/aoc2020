import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc02 {
  case class Record(policy: Policy, password: String)
  case class Policy(letter: String, firstNumber: Int, secondNumber: Int)

  def solve(input: List[String], validationMethod: Record => Boolean): Int = {
    input.map(parse).count(validationMethod)
  }

  def isValidOld(record: Record): Boolean = {
    val policyLetterOccurrences = record.password.toCharArray.count(_.toString == record.policy.letter)
    policyLetterOccurrences >= record.policy.firstNumber && policyLetterOccurrences <= record.policy.secondNumber
  }

  def isValidNew(record: Record): Boolean = {
    val firstLetter = record.password.charAt(record.policy.firstNumber - 1).toString
    val secondLetter = record.password.charAt(record.policy.secondNumber - 1).toString
    firstLetter == record.policy.letter ^ secondLetter == record.policy.letter
  }

  def parse(input: String): Record = {
    val split = input.split("- :".toCharArray).filter(_.nonEmpty)
    Record(Policy(split(2), split(0).toInt, split(1).toInt), split(3))
  }
}

class Aoc02Tests extends TestBase {
  test("Ex 1") {
    val input =
      """1-3 a: abcde
        |1-3 b: cdefg
        |2-9 c: ccccccccc""".stripMargin

    Aoc02.solve(textToStrings(input), Aoc02.isValidOld) shouldBe 2
  }

  test("Puzzle 1") {
    Aoc02.solve(readStrings("Aoc02.txt"), Aoc02.isValidOld) shouldBe 424
  }

  test("Ex 2") {
    val input =
      """1-3 a: abcde
        |1-3 b: cdefg
        |2-9 c: ccccccccc""".stripMargin

    Aoc02.solve(textToStrings(input), Aoc02.isValidNew) shouldBe 1
  }

  test("Puzzle 2") {
    Aoc02.solve(readStrings("Aoc02.txt"), Aoc02.isValidNew) shouldBe 747
  }
}
