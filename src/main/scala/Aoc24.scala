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

  def passDay(day: Int): Unit = {
    val (tilesToAdd, tilesToRemove) = getTilesToConsider.partition(newStateIsBlack)
    tilesToAdd.foreach(tiles.add)
    tilesToRemove.foreach(tiles.remove)
  }

  def getTilesToConsider: List[Pair] = tiles.flatMap(tile => List(tile) ::: getNeighbours(tile)).toList.distinct

  def newStateIsBlack(tile: Pair): Boolean = if(tiles.contains(tile)) staysBlack(tile) else becomesBlack(tile)

  def staysBlack(tile: Pair): Boolean = {
    val numBlackNeighbours = getNumBlackNeighbours(tile)
    if (numBlackNeighbours == 0 || numBlackNeighbours > 2) false else true
  }

  def becomesBlack(tile: Pair): Boolean = if (getNumBlackNeighbours(tile) == 2) true else false

  def getNumBlackNeighbours(tile: Pair): Int = getNeighbours(tile).count(tiles.contains)

  def getNeighbours(tile: Pair): List[Pair] =
    List(tile.add(2, 0), tile.add(-2, 0), tile.add(-1, 1), tile.add(1, 1), tile.add(-1, -1), tile.add(1, -1))

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