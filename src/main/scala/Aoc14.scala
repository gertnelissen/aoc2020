import java.lang.Long.{parseLong, toBinaryString}
import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

object Aoc14 {
  def memorySum(input: List[String]): Long = {
    new Aoc14(input).memorySum()
  }

  def memorySumV2(input: List[String]): Long = {
    new Aoc14v2(input).memorySum()
  }
}

trait Mask {
  def applyTo(s: String): String
}

class MaskV1(mask: String) extends Mask {
  var andMask: Long = parseLong(mask.replace('X', '1'), 2)
  var orMask: Long = parseLong(mask.replace('X', '0'), 2)

  def applyTo(value: String): String = ((parseLong(value, 10) & andMask) | orMask).toString
}

class MaskV2(mask: String) extends Mask {
  def applyTo(value: String): String = {
    val binary = toBinaryString(value.toInt).reverse.padTo(36, '0').reverse
    binary.indices.map(i => if(mask.charAt(i) != '0') mask.charAt(i) else binary.charAt(i)).mkString
  }
}

class Aoc14(input: List[String]) {
  val memory: mutable.Map[Long, Long] = mutable.Map[Long, Long]()
  var mask: Mask = null

  def memorySum(): Long = {
    input.foreach(handleInstruction)
    memory.values.sum
  }

  def handleInstruction(instruction: String): Unit = {
    if(instruction.startsWith("mask ="))
      handleMask(instruction.drop(7))
    else {
      val memoryLocation = instruction.substring(instruction.indexOf('[') + 1, instruction.indexOf(']')).toInt
      val value = instruction.substring(instruction.indexOf('=') + 2)
      handleMemoryInstruction(memoryLocation, value)
    }
  }

  def handleMask(maskString: String): Unit = mask = new MaskV1(maskString)

  def handleMemoryInstruction(memoryLocation: Int, value: String): Unit = {
    memory.update(memoryLocation, mask.applyTo(value).toLong)
  }
}

class Aoc14v2(input: List[String]) extends Aoc14(input) {
  override def handleMask(maskString: String): Unit = mask = new MaskV2(maskString)

  override def handleMemoryInstruction(memoryLocation: Int, value: String): Unit = {
    toRealLocations(mask.applyTo(memoryLocation.toString)).foreach(memory.update(_, value.toLong))
  }

  def toRealLocations(memoryLocation: String): List[Long] = {
    if(!memoryLocation.contains('X'))
      return List(parseLong(memoryLocation, 2))

    List(
      toRealLocations(memoryLocation.replaceFirst("X", "0")),
      toRealLocations(memoryLocation.replaceFirst("X", "1"))
    ).flatten
  }
}

class Aoc14Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0""".stripMargin)

    Aoc14.memorySum(input) shouldBe 165
  }

  test("Puzzle 1") { Aoc14.memorySum(readStrings("Aoc14.txt")) shouldBe 15172047086292L }

  test("Ex 2") {
    val input = textToStrings(
      """mask = 000000000000000000000000000000X1001X
        |mem[42] = 100
        |mask = 00000000000000000000000000000000X0XX
        |mem[26] = 1""".stripMargin)

    Aoc14.memorySumV2(input) shouldBe 208
  }

  test("Puzzle 2") { Aoc14.memorySumV2(readStrings("Aoc14.txt")) shouldBe 4197941339968L }
}