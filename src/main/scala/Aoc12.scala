import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scala.math.abs

object Aoc12 {
  def manhattanDistance(input: List[String]): Int = {
    val ship = new SimpleShip()
    input.foreach(ship.handleInstruction)
    ship.manhattanDistance
  }

  def manhattanDistanceAdvanced(input: List[String]): Int = {
    val ship = new WaypointShip(Pair(10, -1))
    input.foreach(ship.handleInstruction)
    ship.manhattanDistance
  }
}

trait Ship {
  var position: Pair = Pair(0, 0)

  def handleInstruction(instruction: String): Unit = {
    instruction.charAt(0) match {
      case 'N' => handleNorth(instruction.drop(1).toInt)
      case 'S' => handleSouth(instruction.drop(1).toInt)
      case 'E' => handleEast(instruction.drop(1).toInt)
      case 'W' => handleWest(instruction.drop(1).toInt)
      case 'R' => handleRotateRight(instruction.drop(1).toInt)
      case 'L' => handleRotateLeft(instruction.drop(1).toInt)
      case 'F' => handleForward(instruction.drop(1).toInt)
    }
  }

  def manhattanDistance: Int = abs(position.x) + abs(position.y)

  def handleNorth(n: Int)
  def handleSouth(n: Int)
  def handleEast(n: Int)
  def handleWest(n: Int)
  def handleRotateRight(n: Int)
  def handleRotateLeft(n: Int)
  def handleForward(n: Int)
}

class SimpleShip extends Ship {
  var direction: Int = 0

  override def handleNorth(n: Int): Unit = position = position.add(0, -n)
  override def handleSouth(n: Int): Unit = position = position.add(0, n)
  override def handleEast(n: Int): Unit = position = position.add(n, 0)
  override def handleWest(n: Int): Unit = position = position.add(-n, 0)
  override def handleRotateRight(n: Int): Unit = direction = (direction + n + 360) % 360
  override def handleRotateLeft(n: Int): Unit = direction = (direction - n + 360) % 360
  override def handleForward(n: Int): Unit =
    direction match {
      case 0 => handleEast(n)
      case 90 => handleSouth(n)
      case 180 => handleWest(n)
      case 270 => handleNorth(n)
    }
}

class WaypointShip(initialWayPoint: Pair) extends Ship {
  var wayPoint: Pair = initialWayPoint

  override def handleNorth(n: Int): Unit = wayPoint = wayPoint.add(0, -n)
  override def handleSouth(n: Int): Unit = wayPoint = wayPoint.add(0, n)
  override def handleEast(n: Int): Unit = wayPoint = wayPoint.add(n, 0)
  override def handleWest(n: Int): Unit = wayPoint = wayPoint.add(-n, 0)
  override def handleRotateRight(n: Int): Unit = wayPoint = rotate(wayPoint, n)
  override def handleRotateLeft(n: Int): Unit = wayPoint = rotate(wayPoint, -n)
  override def handleForward(n: Int): Unit = position = position.add(wayPoint.x * n, wayPoint.y * n)

  def rotate(pos: Pair, angle: Int): Pair =
    (angle + 360) % 360 match {
      case 90 => Pair(-pos.y, pos.x)
      case 180 => Pair(-pos.x, -pos.y)
      case 270 => Pair(pos.y, -pos.x)
    }
}

class Aoc12Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """F10
        |N3
        |F7
        |R90
        |F11""".stripMargin)

    Aoc12.manhattanDistance(input) shouldBe 25
  }

  test("Puzzle 1") {
    Aoc12.manhattanDistance(readStrings("Aoc12.txt")) shouldBe 1007
  }

  test("Ex 2") {
    val input = textToStrings(
      """F10
        |N3
        |F7
        |R90
        |F11""".stripMargin)

    Aoc12.manhattanDistanceAdvanced(input) shouldBe 286
  }

  test("Puzzle 2") {
    Aoc12.manhattanDistanceAdvanced(readStrings("Aoc12.txt")) shouldBe 41212
  }
}