import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc01 {
  def solve(input: List[Int], target: Int, combinationSize: Int = 2): Int = {
    input.combinations(combinationSize).filter(_.sum == target).nextOption().get.product
  }
}

class Aoc01Tests extends TestBase {
  test("Ex 1") {
    val input =
      """1721
        |979
        |366
        |299
        |675
        |1456""".stripMargin

    Aoc01.solve(textToInts(input), 2020) shouldBe 514579
  }

  test("Puzzle 1") {
    Aoc01.solve(readInts("Aoc01.txt"), 2020) shouldBe 1016619
  }

  test("Ex 2") {
    val input =
      """1721
        |979
        |366
        |299
        |675
        |1456""".stripMargin

    Aoc01.solve(textToInts(input), 2020, 3) shouldBe 241861950
  }

  test("Puzzle 2") {
    Aoc01.solve(readInts("Aoc01.txt"), 2020, 3) shouldBe 218767230
  }
}
