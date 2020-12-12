import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc11 {
  def seekEquilibrium(input: List[String]): Int = {
    val solver = new Aoc11(input)
    solver.seekEquilibrium()
  }

  def seekEquilibriumB(input: List[String]): Int = {
    val solver = new Aoc11b(input)
    solver.seekEquilibrium()
  }
}

class Aoc11b(input: List[String]) extends Aoc11(input) {
  override def determineNewStatus(field: (Pair, Char)): Char = {
    field._2 match {
      case '.' => '.'
      case 'L' =>
        if(countVisibleOccupiedSeats(field._1) > 0)
          'L' else '#'
      case '#' =>
        if(countVisibleOccupiedSeats(field._1) >= 5)
          'L' else '#'
    }
  }

  def countVisibleOccupiedSeats(pos: Pair): Int = {
    (-1 to 1).flatMap(x => (-1 to 1).map(y => (x, y)))
      .filterNot(direction => direction._1 == 0 && direction._2 == 0)
      .count(direction => seesOccupiedSeatInDirection(pos, direction))
  }

  def seesOccupiedSeatInDirection(pos: Pair, direction: (Int, Int)): Boolean = {
    var newPos = pos.add(direction)

    while(!outOfBounds(newPos)) {
      getField(newPos) match {
        case '#' => return true
        case 'L' => return false
        case '.' => newPos = newPos.add(direction)
      }
    }

    false
  }

  def outOfBounds(pos: Pair): Boolean = {
    pos.x < 0 || pos.x >= numCols || pos.y < 0 || pos.y >= numRows
  }
}

class Aoc11(input: List[String]) extends ImmutableHashMapGrid {
  this.parse(input)

  val numRows: Int = input.length
  val numCols: Int = input.head.length

  def seekEquilibrium(): Int = {
    while(takeStep()) {}
    countTakenSeats()
  }

  def takeStep(): Boolean = {
    val newMap = map.map(stepField)
    val hasMoved = isDifferent(map, newMap)
    map = newMap
    hasMoved
  }

  def isDifferent(newMap: Map[Pair, Char], oldMap: Map[Pair, Char]): Boolean = {
    newMap.exists(field => oldMap(field._1) != field._2)
  }

  def stepField(field: (Pair, Char)): (Pair, Char) = {
    (field._1, determineNewStatus(field))
  }

  def determineNewStatus(field: (Pair, Char)): Char = {
    field._2 match {
      case '.' => '.'
      case 'L' =>
        if(countOccupiedSurroundingSeats(field._1).exists(_ == '#'))
          'L' else '#'
      case '#' =>
        if(countOccupiedSurroundingSeats(field._1).count(_ == '#') >= 4)
          'L' else '#'
    }
  }

  def countOccupiedSurroundingSeats(pos: Pair): Iterable[Char] = {
    (-1 to 1).flatMap(x => (-1 to 1).map(y => (x, y)))
      .filterNot(adjustment => adjustment._1 == 0 && adjustment._2 == 0)
      .map(adjustment => pos.add(adjustment))
      .filterNot(newPos => newPos.x < 0 || newPos.y < 0)
      .filterNot(newPos => newPos.x >= numCols || newPos.y >= numRows)
      .map(getField)
  }

  def countTakenSeats(): Int = {
    countsPerCharacter().getOrElse('#', 0)
  }
}

class Aoc11Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """L.LL.LL.LL
        |LLLLLLL.LL
        |L.L.L..L..
        |LLLL.LL.LL
        |L.LL.LL.LL
        |L.LLLLL.LL
        |..L.L.....
        |LLLLLLLLLL
        |L.LLLLLL.L
        |L.LLLLL.LL""".stripMargin)

    Aoc11.seekEquilibrium(input) shouldBe 37
  }

  test("Puzzle 1") {
    Aoc11.seekEquilibrium(readStrings("Aoc11.txt")) shouldBe 2346
  }

  test("Ex 2") {
    val input = textToStrings(
      """L.LL.LL.LL
        |LLLLLLL.LL
        |L.L.L..L..
        |LLLL.LL.LL
        |L.LL.LL.LL
        |L.LLLLL.LL
        |..L.L.....
        |LLLLLLLLLL
        |L.LLLLLL.L
        |L.LLLLL.LL""".stripMargin)

    Aoc11.seekEquilibriumB(input) shouldBe 26
  }

  test("Puzzle 2") {
    Aoc11.seekEquilibriumB(readStrings("Aoc11.txt")) shouldBe 2111
  }
}