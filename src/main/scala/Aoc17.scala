import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

object Aoc17 {
  def simulate(input: List[String], cycles: Int): Int = {
    val game = new Aoc17(input, false)
    game.takeSteps(6)
    game.count('#')
  }

  def simulate4d(input: List[String], cycles: Int): Int = {
    val game = new Aoc17(input, true)
    game.takeSteps(6)
    game.count('#')
  }
}

class Aoc17(input: List[String], useFourthDim: Boolean) extends Automata {
  override var activeKey = '#'
  override var passiveKey = '.'
  override var fourthDim: Boolean = useFourthDim

  override var activeStaysActiveRules: List[Rule] = List(
    new Rule() {
      override def shouldBeActive(pos: Position): Boolean = {
        val environment = getEnvironment(pos)
        val activeNeighbours = environment.count(field => isActive(field._2))
        activeNeighbours == 2 || activeNeighbours == 3
      }
    }
  )

  override var passiveBecomesActiveRules: List[Rule] = List(
    new Rule() {
      override def shouldBeActive(pos: Position): Boolean = {
        val environment = getEnvironment(pos)
        val activeNeighbours = environment.count(field => isActive(field._2))
        activeNeighbours == 3
      }
    }
  )

  parse(input)

  def parse(input: List[String]): Unit = {
    input.zipWithIndex.foreach(row => parseRow(row._2, row._1))
  }

  def parseRow(rowIndex: Int, input: String): Unit = {
    input.toCharArray.zipWithIndex.filter(col => isActive(col._1)).foreach(col => setField(col._2, rowIndex, 0, 0, col._1))
  }

  override def isActive(c: Char): Boolean = c == activeKey
}

trait Automata {
  type Value = Char

  case class Position(x: Int, y: Int, z: Int, w: Int)

  trait Rule {
    def shouldBeActive(pos: Position): Boolean
  }

  val fields: mutable.Map[Position, Value] = mutable.Map[Position, Value]()

  var activeKey: Char
  var passiveKey: Char
  var fourthDim: Boolean

  var activeStaysActiveRules: List[Rule]
  var passiveBecomesActiveRules: List[Rule]

  def setField(x: Int, y: Int, z: Int, w: Int, value: Char): Unit = fields.update(Position(x, y, z, w), value)

  def takeSteps(numSteps: Int): Unit = (1 to numSteps).foreach(takeStep)

  def takeStep(stepId: Int): Unit = {
    val newFields = getAllRelatedFields.map(field => {
      val newValue = if (isActive(field._2)) processActiveField(field._1) else processPassiveField(field._1)
      field._1 -> newValue
    })

    newFields.filter(field => isActive(field._2) || fields.contains(field._1)).foreach(field =>
      fields.update(field._1, field._2)
    )
  }

  def getAllRelatedFields: IndexedSeq[(Position, Value)] = {
    (minX - 1 to maxX + 1).flatMap(x => {
      (minY - 1 to maxY + 1).flatMap(y => {
        (minZ - 1 to maxZ + 1).flatMap(z => {
          (actualMinW(minW) to actualMaxW(maxW)).map(w => {
            val pos = Position(x, y, z, w)
            (pos, fields.getOrElse(pos, passiveKey))
          })
        })
      })
    })
  }

  def minX: Int = fields.map(_._1.x).min
  def maxX: Int = fields.map(_._1.x).max
  def minY: Int = fields.map(_._1.y).min
  def maxY: Int = fields.map(_._1.y).max
  def minZ: Int = fields.map(_._1.z).min
  def maxZ: Int = fields.map(_._1.z).max
  def minW: Int = fields.map(_._1.w).min
  def maxW: Int = fields.map(_._1.w).max
  def actualMinW(w: Int): Int = if(fourthDim) w - 1 else w
  def actualMaxW(w: Int): Int = if(fourthDim) w + 1 else w

  def getEnvironment(pos: Position): IndexedSeq[(Position, Value)] = {
    (pos.x - 1 to pos.x + 1).flatMap(x => {
      (pos.y - 1 to pos.y + 1).flatMap(y => {
        (pos.z - 1 to pos.z + 1).flatMap(z => {
          (actualMinW(pos.w) to actualMaxW(pos.w)).map(w => {
            val posToCheck = Position(x, y, z, w)
            (posToCheck, fields.getOrElse(posToCheck, passiveKey))
          })
        })
      })
    }).filterNot(field => field._1 == pos)
  }

  def processActiveField(pos: Position): Char = {
    if(activeStaysActiveRules.exists(rule => rule.shouldBeActive(pos))) activeKey else passiveKey
  }

  def processPassiveField(pos: Position): Char = {
    if(passiveBecomesActiveRules.exists(rule => rule.shouldBeActive(pos))) activeKey else passiveKey
  }

  def count(c: Char): Int = fields.values.count(_ == c)

  def isActive(c: Char): Boolean

  def print(): Unit = {
    (minW to maxW).foreach(w => (minZ to maxZ).foreach(z => {
      println(s"Z=$z, W=$w")
      print(z, w)
    }))
  }

  def print(z: Int, w: Int): Unit = {
    (minY to maxY).foreach(y => {
      println((minX to maxX).map(x => fields.getOrElse(Position(x, y, z, w), passiveKey)).mkString)
    })
  }
}

class Aoc17Tests extends TestBase {
  test("Ex 1") {
    Aoc17.simulate(textToStrings(""".#.
                                |..#
                                |###""".stripMargin), 6) shouldBe 112
  }

  test("Puzzle 1") {
    Aoc17.simulate(readStrings("Aoc17.txt"), 6) shouldBe 362
  }

  test("Ex 2") {
    Aoc17.simulate4d(textToStrings(""".#.
                                   |..#
                                   |###""".stripMargin), 6) shouldBe 848
  }

  test("Puzzle 2") {
    Aoc17.simulate4d(readStrings("Aoc17.txt"), 6) shouldBe 1980
  }
}