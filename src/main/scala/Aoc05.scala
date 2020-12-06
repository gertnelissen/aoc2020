import java.lang.Integer.parseInt

import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc05 {
  def seatId(pass: String): Int = {
    parseInt(toBinary(pass), 2)
  }

  def maxSeatId(passes: List[String]): Int = {
    passes.map(seatId).max
  }

  def mySeatId(passes: List[String]): Int = {
    passes.map(seatId).sorted.sliding(2).find(window => window.head + 1 != window(1)).get.head + 1
  }

  def toBinary(pass: String): String = {
    pass
      .replace('B', '1')
      .replace('F', '0')
      .replace('R', '1')
      .replace('L', '0')
  }
}

class Aoc05Tests extends TestBase {
  test("Ex 1") {
    Aoc05.seatId("FBFBBFFRLR") shouldBe 357
  }

  test("Puzzle 1") {
    Aoc05.maxSeatId(readStrings("Aoc05.txt")) shouldBe 996
  }

  test("Puzzle 2") {
    Aoc05.mySeatId(readStrings("Aoc05.txt")) shouldBe 671
  }
}