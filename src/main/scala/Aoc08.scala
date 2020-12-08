import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

object Aoc08 {
  def registerAtLoop(input: List[String]): Int = {
    val solver = new Aoc08(input)
    solver.runUntilHalted()
    solver.state.register
  }

  def checkHalts(input: List[String]): Int = {
    val solver = new Aoc08(input)
    solver.detectInstructionToChangeToTerminate()
  }
}

class Aoc08(input: List[String]) extends Program(input) {
  var pastInstructions: mutable.ListBuffer[Int] = mutable.ListBuffer[Int]()

  def runUntilHalted(): Unit = {
    reset()
    do step() while(!halt())
  }

  def halt(): Boolean = loopDetected || outOfBounds

  def loopDetected: Boolean = pastInstructions.contains(state.head)

  def outOfBounds: Boolean = state.head > instructions.keys.max

  def reset(): Unit = {
    pastInstructions.clear()
    state = State(0, 0)
  }

  override def step(): Unit = {
    pastInstructions += state.head
    super.step()
  }

  def detectInstructionToChangeToTerminate(): Int = {
    for(instruction <- instructionsToChange()) {
      attemptSwitch(instruction)
      if(outOfBounds)
        return state.register
    }

    throw new Exception("No solution found")
  }

  def attemptSwitch(instruction: Int): Unit = {
    val oldInstructions = instructions
    switchInstruction(instruction)
    runUntilHalted()

    if(outOfBounds)
      return

    instructions = oldInstructions
  }

  def switchInstruction(instruction: Int): Unit = {
    val switchedInstruction = instructions(instruction) match {
      case NopInstruction(operand) => JmpInstruction(operand)
      case JmpInstruction(operand) => NopInstruction(operand)
      case _ => throw new IllegalArgumentException("Can't switch instruction %s".format(instructions(instruction)))
    }

    instructions = instructions.updated(instruction, switchedInstruction)
  }

  def instructionsToChange(): List[Int] = {
    instructions.filter(instruction =>
      instruction._2.isInstanceOf[NopInstruction] || instruction._2.isInstanceOf[JmpInstruction]
    ).keys.toList
  }
}

class Aoc08Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """nop +0
        |acc +1
        |jmp +4
        |acc +3
        |jmp -3
        |acc -99
        |acc +1
        |jmp -4
        |acc +6""".stripMargin)

    Aoc08.registerAtLoop(input) shouldBe 5
  }

  test("Puzzle 1") {
    Aoc08.registerAtLoop(readStrings("Aoc08.txt")) shouldBe 1709
  }

  test("Ex 2") {
    val input = textToStrings(
      """nop +0
        |acc +1
        |jmp +4
        |acc +3
        |jmp -3
        |acc -99
        |acc +1
        |jmp -4
        |acc +6""".stripMargin)

    Aoc08.checkHalts(input) shouldBe 8
  }

  test("Puzzle 2") {
    Aoc08.checkHalts(readStrings("Aoc08.txt")) shouldBe 1976
  }
}