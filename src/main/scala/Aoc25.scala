import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scala.Iterator._

class Aoc25(cardKey: Long, doorKey: Long) {
  def solve(): Long = calcKey(doorKey, calcLoopSize(cardKey))

  def calcLoopSize(key: Long): Int = calc(7).find(_._1 == key).get._2

  def calcKey(subject: Long, loopSize: Int): Long = calc(subject).find(_._2 == loopSize).get._1

  def calc(subject: Long): Iterator[(Long, Int)] = iterate((1L, 0))(i => ((i._1 * subject) % 20201227L, i._2 + 1))
}

class Aoc25Tests extends TestBase {
  test("Ex") { new Aoc25(5764801L, 17807724L).solve() shouldBe 14897079L }

  test("Puzzle") { new Aoc25(11349501L, 5107328L).solve() shouldBe 7936032L }
}