import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

class Aoc09 {
  type Validation = (Long, Boolean)

  def findBreaker(input: List[String], preambleSize: Int): Long = {
    input.map(_.toLong).sliding(preambleSize + 1).map(validate).find(result => !result._2).get._1
  }

  def findSet(input: List[String], target: Long): Long = {
    Iterator.range(2, input.length)
      .map(setSize => input.map(_.toLong).sliding(setSize).find(subSet => subSet.sum == target))
      .find(_.isDefined).get
      .map(solutionSet => solutionSet.min + solutionSet.max).get
  }

  def validate(input: List[Long]): Validation = {
    val target = input.reverse.head
    val list = input.dropRight(1)
    (target, sumsUpToTarget(list, target))
  }

  def sumsUpToTarget(options: List[Long], target: Long): Boolean = {
    options.combinations(2).exists(tuple => tuple.sum == target)
  }
}

class Aoc09Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """35
        |20
        |15
        |25
        |47
        |40
        |62
        |55
        |65
        |95
        |102
        |117
        |150
        |182
        |127
        |219
        |299
        |277
        |309
        |576""".stripMargin)

    new Aoc09().findBreaker(input, 5) shouldBe 127
  }

  test("Puzzle 1") {
    new Aoc09().findBreaker(readStrings("Aoc09.txt"), 25) shouldBe 258585477
  }

  test("Ex 2") {
    val input = textToStrings(
      """35
        |20
        |15
        |25
        |47
        |40
        |62
        |55
        |65
        |95
        |102
        |117
        |150
        |182
        |127
        |219
        |299
        |277
        |309
        |576""".stripMargin)

    new Aoc09().findSet(input, 127) shouldBe 62
  }

  test("Puzzle 2") {
    new Aoc09().findSet(readStrings("Aoc09.txt"), 258585477) shouldBe 36981213
  }
}