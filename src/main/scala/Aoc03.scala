import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc03 {
  def countEncounteredTreesWithSlope(input: List[String], slope: Pair): Int = {
    val solver = new Aoc03()
    solver.parse(input)
    solver.travelToBottom(slope)
    solver.numEncounteredTrees()
  }
}

class Aoc03 extends MapGrid {
  var initialSize: Pair = Pair(0, 0)

  def setInitialSize(): Unit = initialSize = Pair(maxX + 1, maxY + 1)

  override def parse(input: List[String]): Unit = {
    super.parse(input)
    setInitialSize()
  }

  def travelToBottom(slope: Pair): Unit = {
    var pos = Pair(0, 0)

    while(pos.y < initialSize.subtract(slope).y)  {
      pos = pos.add(slope)
      markLocation(pos)
    }
  }

  def markLocation(pos: Pair): Unit = setField(pos, if(getField(pos) == '#') 'X' else 'O')

  override def getField(pair: Pair): Char = {
    if(map.isDefinedAt(pair))
      map(pair)
    else {
      val boundedX = pair.x % initialSize.x
      translateToOriginal(map.getOrElse(Pair(boundedX, pair.y), '?'))
    }
  }

  // Used for printing
  def translateToOriginal(c: Char): Char = {
    c match {
      case 'O' => '.'
      case 'X' => '#'
      case other => other
    }
  }

  def numEncounteredTrees(): Int = map.values.toList.count(_ == 'X')
}

class Aoc03Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """..##.......
        |#...#...#..
        |.#....#..#.
        |..#.#...#.#
        |.#...##..#.
        |..#.##.....
        |.#.#.#....#
        |.#........#
        |#.##...#...
        |#...##....#
        |.#..#...#.#""".stripMargin)

    Aoc03.countEncounteredTreesWithSlope(input, Pair(3, 1)) shouldBe 7
  }

  test("Puzzle 1") {
    Aoc03.countEncounteredTreesWithSlope(readStrings("Aoc03.txt"), Pair(3, 1)) shouldBe 286
  }

  test("Ex 2") {
    val input = textToStrings(
      """..##.......
        |#...#...#..
        |.#....#..#.
        |..#.#...#.#
        |.#...##..#.
        |..#.##.....
        |.#.#.#....#
        |.#........#
        |#.##...#...
        |#...##....#
        |.#..#...#.#""".stripMargin)

    val slopes = List(Pair(1, 1), Pair(3, 1), Pair(5, 1), Pair(7, 1), Pair(1, 2))
    slopes.map(slope => Aoc03.countEncounteredTreesWithSlope(input, slope)).product shouldBe 336
  }

  test("Puzzle 2") {
    val input = readStrings("Aoc03.txt")
    val slopes = List(Pair(1, 1), Pair(3, 1), Pair(5, 1), Pair(7, 1), Pair(1, 2))
    slopes.map(slope => Aoc03.countEncounteredTreesWithSlope(input, slope).toLong).product shouldBe 3638606400L
  }
}