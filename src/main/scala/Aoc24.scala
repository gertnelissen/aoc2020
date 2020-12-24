import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import Util._
import scala.collection.mutable

class Aoc24(input: List[String]) {
  case class Dimensions(min: Pair, max: Pair)

  var tiles: mutable.Set[Pair] = mutable.Set[Pair]()

  def numBlackAfter(days: Int): Int = {
    input.foreach(processInstruction)
    (1 to days).foreach(passDay)
    blackTiles
  }

  def blackTiles: Int = tiles.size

  def passDay(day: Int): Unit =
    tilesToConsider.map(tile => (tile, newStateIsBlack(tile))).foreach(tile =>
      if(tile._2) tiles.add(tile._1) else tiles.remove(tile._1))

  def tilesToConsider: List[Pair] = tiles.flatMap(tile => List(tile) ::: getNeighbours(tile)).toList.distinct

  def newStateIsBlack(tile: Pair): Boolean = {
    val numBlackNeighbours = getNeighbours(tile).count(tiles.contains)
    if (tiles.contains(tile))
      if (numBlackNeighbours == 0 || numBlackNeighbours > 2) false else true
    else
      if (numBlackNeighbours == 2) true else false
  }

  def getNeighbours(pair: Pair): List[Pair] =
    List(pair.add(2, 0), pair.add(-2, 0), pair.add(-1, 1), pair.add(1, 1), pair.add(-1, -1), pair.add(1, -1))

  def processInstruction(instruction: String): Unit = {
    val tilePosition = getTilePosition(instruction)

    if(tiles.contains(tilePosition))
      tiles.remove(tilePosition)
    else
      tiles.add(tilePosition)
  }

  def getTilePosition(instruction: String): Pair = {
    var steps = instruction
    var tilePosition = Pair(0, 0)

    while(steps.nonEmpty) {
      steps.head match {
        case 'e' =>
          tilePosition = tilePosition.add(2, 0)
          steps = steps.drop(1)
        case 'w' =>
          tilePosition = tilePosition.add(-2, 0)
          steps = steps.drop(1)
        case 'n' =>
          steps(1) match {
            case 'e' => tilePosition = tilePosition.add(1, 1)
            case 'w' => tilePosition = tilePosition.add(-1, 1)
          }
          steps = steps.drop(2)
        case 's' =>
          steps(1) match {
            case 'e' => tilePosition = tilePosition.add(1, -1)
            case 'w' => tilePosition = tilePosition.add(-1, -1)
          }
          steps = steps.drop(2)
      }
    }

    tilePosition
  }
}

class Aoc24Tests extends TestBase {
  test("Ex 1") { new Aoc24(readStrings("Aoc24example.txt")).numBlackAfter(0) shouldBe 10 }

  test("Puzzle 1") { new Aoc24(readStrings("Aoc24.txt")).numBlackAfter(0) shouldBe 356 }

  test("Ex 2") { new Aoc24(readStrings("Aoc24example.txt")).numBlackAfter(100) shouldBe 2208 }

  test("Puzzle 2") { new Aoc24(readStrings("Aoc24.txt")).numBlackAfter(100) shouldBe 3887 }
}