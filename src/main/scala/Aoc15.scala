import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scala.collection.mutable

object Aoc15 {
  def produce(input: String, n: Int): Int = {
    val solver = new Aoc15(input.split(',').toList)
    solver.produce(n)
    solver.lastSpoken
  }
}

class Aoc15(input: List[String]) {
  var memory: mutable.HashMap[Int, Int] = mutable.HashMap(input.zipWithIndex.map(entry => entry._1.toInt -> entry._2): _*)
  var lastSpoken: Int = input.last.toInt

  def produce(n: Int): Unit = (input.length until n).foreach(produceIndex)

  def produceIndex(n: Int): Unit = {
    val produce = memory.get(lastSpoken).map(index => (n - 1) - index).getOrElse(0)
    memory.update(lastSpoken, n - 1)
    lastSpoken = produce
  }
}

class Aoc15Tests extends TestBase {
  test("Ex 1") { Aoc15.produce("0,3,6", 2020) shouldBe 436 }

  test("Puzzle 1") { Aoc15.produce("19,20,14,0,9,1", 2020) shouldBe 1325 }

  test("Ex 2") { Aoc15.produce("0,3,6", 30000000) shouldBe 175594 }

  test("Puzzle 2") { Aoc15.produce("19,20,14,0,9,1", 30000000) shouldBe 59006 }
}