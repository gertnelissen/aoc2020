import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scala.collection.mutable

object Aoc23 {
  def playRegular(input: String, rounds: Int): String = {
    val cups = new Cups()
    cups.init(input.toCharArray.map(_.toString.toInt))

    val game = new Aoc23(cups, rounds)
    game.play()
    game.output
  }

  def playGenerated(input: String, cupAmount: Int, rounds: Int): Long = {
    val cups = new Cups()
    val maxNumber = input.toCharArray.map(_.toString.toInt).max
    cups.init(input.toCharArray.map(_.toString.toInt).concat(maxNumber + 1 to cupAmount))

    val game = new Aoc23(cups, rounds)
    game.play()
    game.output2
  }
}

class Link(val value: Int, var next: Link)

class Cups() {
  var maxCup: Int = _
  var index: mutable.HashMap[Int, Link] = mutable.HashMap[Int, Link]()
  var pointer: Link = _

  def init(input: Array[Int]): Unit = {
    maxCup = input.max

    pointer = new Link(input.head, pointer)
    index.update(pointer.value, pointer)
    var lastLink = pointer

    input.indices.drop(1).foreach(elementIndex => {
      val currentLink = new Link(input(elementIndex), pointer)
      index.update(currentLink.value, currentLink)
      lastLink.next = currentLink
      lastLink = currentLink
    })
  }

  def listFrom(n: Int): List[Int] = {
    var current = index(n)
    val initial = current

    Iterator.continually({
      val toOutput = current.next
      current = current.next
      toOutput.value
    }).takeWhile(_ => initial != current).toList
  }

  def doTheThing(): Unit = {
    val next1 = pointer.next
    val next2 = next1.next
    val next3 = next2.next

    // Remove the cups
    pointer.next = next3.next

    // Insert the cups
    val insertionPoint = index(getInsertionPointIndex(List(next1.value, next2.value, next3.value)))
    next3.next = insertionPoint.next
    insertionPoint.next = next1

    pointer = pointer.next
  }

  def getInsertionPointIndex(not: List[Int]): Int = {
    var insertionPoint = pointer.value

    do {
      insertionPoint -= 1
      if(insertionPoint == 0) insertionPoint = maxCup
    } while(not.contains(insertionPoint))

    insertionPoint
  }
}

class Aoc23(cups: Cups, rounds: Int) {
  def play(): Unit = (1 to rounds).foreach(_ => cups.doTheThing())

  def output: String = cups.listFrom(1).mkString

  def output2: Long = cups.index(1).next.value * cups.index(1).next.next.value.toLong
}

class Aoc23Tests extends TestBase {
  test("Ex 1") {
    Aoc23.playRegular("389125467", 10) shouldBe "92658374"
  }

  test("Ex 2") {
    Aoc23.playRegular("389125467", 100) shouldBe "67384529"
  }

  test("Puzzle 1") {
    Aoc23.playRegular("253149867", 100) shouldBe "34952786"
  }

  test("Ex 3") {
    Aoc23.playGenerated("389125467", 1000000, 10000000) shouldBe 149245887792L
  }

  test("Puzzle 2") {
    Aoc23.playGenerated("253149867", 1000000, 10000000) shouldBe 505334281774L
  }
}