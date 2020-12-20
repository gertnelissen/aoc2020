import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

object Aoc20 {
  def solve(input: List[String]): Long = new Aoc20(input).fourCorners.map(_.id).product

  def waterRoughness(input: List[String]): Int = new Aoc20(input).waterRoughness
}

trait GridOperations extends MapGrid {
  var size: Int = -1

  def determinePossibleBorders: List[String] = getAllSides.flatMap(side => List(side, side.reverse))

  def getAllSides: List[String] = List(getTop, getRight, getBottom, getLeft)

  def flip(): Unit = map = map.map(flipField)

  def flipField(field: (Pair, Char)): (Pair, Char) = (Pair((size - 1) - field._1.x, field._1.y), field._2)

  def rotate(rotations: Int): Unit = (0 until rotations).foreach(_ => rotateRight())

  def rotateRight(): Unit = map = map.map(rotateFieldRight)

  def rotateFieldRight(field: (Pair, Char)): (Pair, Char) = (Pair((size - 1) - field._1.y, field._1.x), field._2)

  def getTop: String = map.filter(_._1.y == 0).toList.sortBy(_._1.x).map(_._2).mkString
  def getRight: String = map.filter(_._1.x == size - 1).toList.sortBy(_._1.y).map(_._2).mkString
  def getBottom: String = map.filter(_._1.y == size - 1).toList.sortBy(_._1.x).reverse.map(_._2).mkString
  def getLeft: String =  map.filter(_._1.x == 0).toList.sortBy(_._1.y).reverse.map(_._2).mkString

  def asStrings: List[String] = (0 to size).map(row => map.filter(_._1.y == row).toList.sortBy(_._1.x).map(_._2).mkString).toList
}

class Tile(val id: Long, gridText: Array[String]) extends MapGrid with GridOperations {
  parse(gridText.toList)
  size = gridText.length

  // Placing
  var position: Option[Pair] = None
  var isFlipped: Boolean = false
  var numRotationsToTheRight: Int = 0
}

class Image(mapInput: Map[Pair, Char]) extends GridOperations {
  this.map = mutable.HashMap(mapInput.toSeq: _*)
  size = maxX - minX
}

class Monster extends GridOperations {
  val monsterText: List[String] = textToStrings(
                    """..................#.
                      |#....##....##....###
                      |.#..#..#..#..#..#...""".stripMargin)
  parse(monsterText)
  size = monsterText.length
}

class Aoc20(input: List[String]) {
  val tiles: Array[Tile] = parseFrames(input)
  val pictureDimension: Int = math.sqrt(tiles.length).toInt

  def fourCorners: Array[Tile] = {
    tiles.filter(tile => getMatchingNeighbours(tile).length == 2)
  }

  def waterRoughness: Int = {
    val monster = new Monster()
    val image = orientImageToContainMonsters(assembleImage(), monster)
    eraseAllMonsters(image, monster)
    image.map.values.count(_ == '#')
  }

  def eraseAllMonsters(image: Image, monster: Monster): Unit = {
    var monsterPosition: Option[Pair] = None
    do {
      monsterPosition = getMonsterPosition(image, monster)
      if(monsterPosition.isDefined)
        removeMonster(image, monster, monsterPosition.get)
    } while(monsterPosition.isDefined)
  }

  def removeMonster(image: Image, monster: Monster, monsterPosition: Pair): Unit = {
    monster.map.filter(_._2 == '#').toList.sortBy(_._1.x).foreach(pos => {
      val fieldToErase = pos._1.add(monsterPosition).subtract(Pair(1, 0))
      image.setField(fieldToErase, 'O')
    })
  }

  def orientImageToContainMonsters(image: Image, monster: Monster): Image = {
    (0 to 1).foreach(flipOrNot => {
      if(flipOrNot == 1)
        image.flip()

      (0 to 3).foreach(rotations => {
        if(rotations > 0)
          image.rotate(1)

        if(getMonsterPosition(image, monster).isDefined)
          return image
      })
    })

    throw new Exception("No monster found in image...")
  }

  def getMonsterPosition(image: Image, monster: Monster): Option[Pair] = {
    val imageStrings = image.asStrings
    val monsterStrings = monster.asStrings

    (0 to imageStrings.length - monsterStrings.length).foreach(possibleMonsterY =>
      (0 to imageStrings.head.length - monsterStrings.head.length).foreach(possibleMonsterX =>
        if(monsterSeenAt(imageStrings, monsterStrings, possibleMonsterX, possibleMonsterY))
          return Some(Pair(possibleMonsterX, possibleMonsterY))))

    None
  }

  def monsterSeenAt(imageStrings: List[String], monsterStrings: List[String], monsterX: Int, monsterY: Int): Boolean = {
    monsterStrings.indices.forall(row =>
      monsterStrings(row).indices.forall(col =>
        if(monsterStrings(row).charAt(col) == '.') true
        else imageStrings(monsterY + row).charAt(monsterX + col) == monsterStrings(row).charAt(col)))
  }

  def combine(): Image = {
    new Image(tiles.flatMap(tile =>
      tile.map
        .filterNot(field => field._1.x == 0 || field._1.y == 0)
        .filterNot(field => field._1.x == tile.size - 1 || field._1.y == tile.size - 1)
        .map(field =>
          (Pair(field._1.x - 1 + tile.position.get.x * (tile.size - 2),
            field._1.y - 1 + tile.position.get.y * (tile.size - 2)), field._2))).toMap)
  }

  def assembleImage(): Image = {
    (0 until pictureDimension).foreach(y =>
      (0 until pictureDimension).foreach(x =>
        if (x == 0 && y == 0) setFirstCorner()
        else determineTileAt(x, y)))

    combine()
  }

  def setFirstCorner(): Unit = {
    (0 to 1).foreach(flipOrNot => {
      (0 to 3).foreach(numRotations => {
        val flip = if(flipOrNot == 0) false else true
        val topLeftTiles = determineTopLeftTilesForGivenFlipAndRotation(flip, numRotations)

        if (topLeftTiles.nonEmpty) {
          val topLeftTile = topLeftTiles.head
          if(flip)
            topLeftTile.flip()
          topLeftTile.rotate(numRotations)
          topLeftTile.position = Some(Pair(0, 0))
          return
        }
      })
    })

    throw new Exception("Top left tile not found :(")
  }

  def determineTopLeftTilesForGivenFlipAndRotation(flip: Boolean, rotations: Int): Array[Tile] = {
    fourCorners.filter(corner => {
      val bottomSide = getBottomSideForGivenFlipAndRotation(corner, flip, rotations)
      val rightSide = getRightSideForGivenFlipAndRotation(corner, flip, rotations)

      tiles.filterNot(_.id == corner.id)
        .count(otherTile => otherTile.determinePossibleBorders.contains(bottomSide)
          || otherTile.determinePossibleBorders.contains(rightSide)) == 2
    })
  }

  def getBottomSideForGivenFlipAndRotation(tile: Tile, flip: Boolean, rotations: Int): String = {
    rotations match {
      case 0 => if(flip) tile.getBottom.reverse else tile.getBottom
      case 1 => if(flip) tile.getRight.reverse else tile.getRight
      case 2 => if(flip) tile.getTop.reverse else tile.getTop
      case 3 => if(flip) tile.getLeft.reverse else tile.getLeft
    }
  }

  // TODO optimize duplication
  def getRightSideForGivenFlipAndRotation(tile: Tile, flip: Boolean, rotations: Int): String = {
    rotations match {
      case 0 => if(flip) tile.getRight.reverse else tile.getRight
      case 1 => if(flip) tile.getTop.reverse else tile.getTop
      case 2 => if(flip) tile.getLeft.reverse else tile.getLeft
      case 3 => if(flip) tile.getBottom.reverse else tile.getBottom
    }
  }

  def isTopLeftTile(tile: Tile): Boolean = {
    tiles
      .filterNot(_.id != tile.id)
      .count(otherTile => otherTile.determinePossibleBorders.contains(tile.getRight) ||
        otherTile.determinePossibleBorders.contains(tile.getBottom)) == 2
  }

  def determineTileAt(x: Int, y: Int): Unit = {
    val sideToMatchWith =
      if (x > 0) tileAt(x - 1, y).getRight
      else tileAt(x, y - 1).getBottom

    val matchingTile = remainingTiles.find(_.determinePossibleBorders.contains(sideToMatchWith)).head
    matchingTile.position = Some(Pair(x, y))

    flipTileIfNecessary(matchingTile, sideToMatchWith)

    if (x > 0) rotateTileSoThatSideIsOnLeft(matchingTile, sideToMatchWith)
    else rotateTileSoThatSideIsOnTop(matchingTile, sideToMatchWith)
  }

  def flipTileIfNecessary(tile: Tile, side: String): Unit = {
    // Check w/ reversed side as we're comparing left to right, to to bottom
    if(!tile.getAllSides.contains(side.reverse))
      tile.flip()
  }

  def rotateTileSoThatSideIsOnLeft(tile: Tile, side: String): Unit = {
    // Check w/ reversed side as we're comparing left to right, to to bottom
    if(side.reverse == tile.getBottom) tile.rotate(1)
    else if(side.reverse == tile.getRight) tile.rotate(2)
    else if(side.reverse == tile.getTop) tile.rotate(3)
  }

  // TODO optimize duplication
  def rotateTileSoThatSideIsOnTop(tile: Tile, side: String): Unit = {
    // Check w/ reversed side as we're comparing left to right, to to bottom
    if(side.reverse == tile.getLeft) tile.rotate(1)
    else if(side.reverse == tile.getBottom) tile.rotate(2)
    else if(side.reverse == tile.getRight) tile.rotate(3)
  }

  def remainingTiles: Array[Tile] = tiles.filter(_.position.isEmpty)

  def tileAt(x: Int, y: Int): Tile = tiles.filter(_.position.contains(Pair(x, y))).head

  def getMatchingNeighbours(tile: Tile): Array[Tile] = {
    tiles.filterNot(_.id == tile.id).filter(otherTile =>
      otherTile.determinePossibleBorders.exists(otherBorder => tile.getAllSides.contains(otherBorder)))
  }

  def parseFrames(input: List[String]): Array[Tile] = {
    input.mkString("--").split("----").map(parseFrame)
  }

  def parseFrame(input: String): Tile = {
    val split = input.split("--")
    new Tile(parseTileId(split(0)), split.drop(1))
  }

  def parseTileId(input: String): Long = input.stripSuffix(":").split(' ')(1).toLong
}

class Aoc20Tests extends TestBase {
  test("Ex 1") {
    Aoc20.solve(readStrings("Aoc20example.txt")) shouldBe 20899048083289L
  }

  test("Puzzle 1") {
    Aoc20.solve(readStrings("Aoc20.txt")) shouldBe 17148689442341L
  }

  test("Ex 2") {
    Aoc20.waterRoughness(readStrings("Aoc20example.txt")) shouldBe 273
  }

  test("Puzzle 2") {
    Aoc20.waterRoughness(readStrings("Aoc20.txt")) shouldBe 2009
  }
}