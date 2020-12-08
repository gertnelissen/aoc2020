trait Instruction {
  val operand: Int
  def process(state: State): State
}

case class NopInstruction(operand: Int) extends Instruction {
  def process(state: State): State = State(state.head + 1, state.register)
}

case class AccInstruction(operand: Int) extends Instruction {
  def process(state: State): State = State(state.head + 1, state.register + operand)
}

case class JmpInstruction(operand: Int) extends Instruction {
  def process(state: State): State = State(state.head + operand, state.register)
}

case class State(head: Int, register: Int) {
  def moved(n: Int): State = {
    State(head + n, register)
  }
}

class Program(input: List[String]) {
  var instructions: Map[Int, Instruction] = parseInstructions(input)
  var state: State = State(0, 0)

  def parseInstructions(input: List[String]): Map[Int, Instruction] = {
    input.map(parseInstruction).zipWithIndex
      .map(instruction => instruction._2 -> instruction._1) .toMap
  }

  def step(): Unit = {
    state = instructions(state.head).process(state)
  }

  def increaseHead(): Unit = state = state.moved(1)

  def parseInstruction(input: String): Instruction = {
    val split = input.split(' ')

    split(0) match {
      case "nop" => NopInstruction(split(1).toInt)
      case "acc" => AccInstruction(split(1).toInt)
      case "jmp" => JmpInstruction(split(1).toInt)
      case _ => throw new IllegalArgumentException("Can't process instruction %s".format(input))
    }
  }

  def printInstructions(): Unit = {
    println(instructions.toList.sortBy(_._1).mkString("\n"))
  }
}
