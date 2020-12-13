import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc13 {
  def earliestBusTime(inputText: List[String]): Long = {
    val (waitTime, busses) = parse(inputText)
    val earliestBus = busses.map(bus => (bus, bus - waitTime % bus)).minBy(_._2)
    earliestBus._1 * earliestBus._2
  }

  def parse(input: List[String]): (Int, List[Int]) = {
    (input.head.toInt, input(1).split(',').filterNot(_ == "x").map(_.toInt).toList)
  }

  def idealBusTimestamp(input: String): Long = {
    val busses = parse(input)

    var t = 0L
    var step = 1L

    busses.foreach(bus => {
      while(t % bus.id != bus.offset)
        t += step

      // A fitting timestamp is found for the current bus, it will now leave every <busId> minutes
      step *= bus.id
    })

    t
  }

  case class Bus(id: Int, offset: Int)

  def parse(input: String): List[Bus] = {
    input.split(',').zipWithIndex.filterNot(_._1 == "x").map(bus => {
      val id = bus._1.toInt
      val offset = (id - bus._2 % id) % id
      Bus(id, offset)
    }).toList
  }
}

class Aoc13Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """939
        |7,13,x,x,59,x,31,19""".stripMargin)

    Aoc13.earliestBusTime(input) shouldBe 295
  }

  test("Puzzle 1") { Aoc13.earliestBusTime(readStrings("Aoc13.txt")) shouldBe 4135 }

  test("Ex 2") { Aoc13.idealBusTimestamp("7,13,x,x,59,x,31,19") shouldBe 1068781 }

  test("Ex 3") { Aoc13.idealBusTimestamp("17,x,13,19") shouldBe 3417 }

  test("Ex 4") { Aoc13.idealBusTimestamp("67,7,59,61") shouldBe 754018 }

  test("Ex 5") { Aoc13.idealBusTimestamp("67,x,7,59,61") shouldBe 779210 }

  test("Ex 6") { Aoc13.idealBusTimestamp("67,7,x,59,61") shouldBe 1261476 }

  test("Ex 7") { Aoc13.idealBusTimestamp("1789,37,47,1889") shouldBe 1202161486 }

  test("Puzzle 2") { Aoc13.idealBusTimestamp(readStrings("Aoc13.txt")(1)) shouldBe 640856202464541L }
}