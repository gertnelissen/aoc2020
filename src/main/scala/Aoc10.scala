import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc10 {
  def gapSizes(input: List[String]): Int = {
    new Aoc10(input).getGapSizes.filter(gapSize => gapSize._1 == 1 || gapSize._1 == 3).values.product
  }

  def reachOptions(input: List[String]): Long = {
    new Aoc10(input).calculateReachOptions.last
  }
}

class Aoc10(adapterInput: List[String], maxJolt: Int = 3) {
  val adapters: List[Int] = adapterInput.map(_.toInt)
  val jolts: List[Int] = (0 :: adapters.max + maxJolt :: adapters).sorted
  var reachOptions: Array[Long] = Array.fill(jolts.length){ 0L }

  def getGapSizes: Map[Int, Int] = {
    jolts.sliding(2).map(gapSize).toList.groupBy(identity).view.mapValues(_.size).toMap
  }

  def gapSize(items: List[Int]): Int = items.max - items.min

  def calculateReachOptions: Array[Long] = {
    reachOptions.update(0, 1L)
    for(adapterPos <- 1 until jolts.length)
      reachOptions.update(adapterPos, getReachOptions(adapterPos))
    reachOptions
  }

  def getReachOptions(position: Int): Long = {
    (1 to maxJolt)
      .map(position - _)
      .filter(inRange)
      .filter(reachable(position, _))
      .map(reachOptions)
      .sum
  }

  def inRange(pos: Int): Boolean = pos >= 0

  def reachable(currentPos: Int, otherPos: Int): Boolean = jolts(currentPos) - jolts(otherPos) <= maxJolt
}

class Aoc10Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """16
        |10
        |15
        |5
        |1
        |11
        |7
        |19
        |6
        |12
        |4""".stripMargin)

    Aoc10.gapSizes(input) shouldBe 35
  }

  test("Ex 2") {
    val input = textToStrings(
      """28
        |33
        |18
        |42
        |31
        |14
        |46
        |20
        |48
        |47
        |24
        |23
        |49
        |45
        |19
        |38
        |39
        |11
        |1
        |32
        |25
        |35
        |8
        |17
        |7
        |9
        |4
        |2
        |34
        |10
        |3""".stripMargin)

    Aoc10.gapSizes(input) shouldBe 220
  }

  test("Puzzle 1") {
    Aoc10.gapSizes(readStrings("Aoc10.txt")) shouldBe 2760
  }

  test("Ex 3") {
    val input = textToStrings(
      """16
        |10
        |15
        |5
        |1
        |11
        |7
        |19
        |6
        |12
        |4""".stripMargin)

    Aoc10.reachOptions(input) shouldBe 8
  }

  test("Ex 4") {
    val input = textToStrings(
      """28
        |33
        |18
        |42
        |31
        |14
        |46
        |20
        |48
        |47
        |24
        |23
        |49
        |45
        |19
        |38
        |39
        |11
        |1
        |32
        |25
        |35
        |8
        |17
        |7
        |9
        |4
        |2
        |34
        |10
        |3""".stripMargin)

    Aoc10.reachOptions(input) shouldBe 19208
  }

  test("Puzzle 2") {
    Aoc10.reachOptions(readStrings("Aoc10.txt")) shouldBe 13816758796288L
  }
}