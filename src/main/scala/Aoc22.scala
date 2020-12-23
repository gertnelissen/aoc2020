import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scala.collection.mutable

object Aoc22 {
  def playClassicGame(input: List[String]): Int = {
    val game = new Aoc22()
    game.parse(input)
    game.playGame()
    game.winningPlayerScore()
  }

  def playRecursiveGame(input: List[String]): Int = {
    val game = new Aoc22Recursive()
    game.parse(input)
    game.playGame()
    game.winningPlayerScore()
  }
}

class Aoc22Recursive(p1cards: mutable.Queue[Int] = mutable.Queue[Int](),
                     p2cards: mutable.Queue[Int] = mutable.Queue[Int]()) extends Aoc22(p1cards, p2cards) {
  var playedGames = new mutable.HashSet[Int]()

  override def playGame(): Int = {
    while(gameIsGoingOn && !loopDetected()) playCard()
    if(p1cards.nonEmpty) 1 else 2
  }

  def loopDetected(): Boolean = {
    val serializedState = p1cards.hashCode()
    if(playedGames.contains(serializedState)) return true
    playedGames.add(serializedState)
    false
  }

  override def playCard(): Unit = {
    val p1card = p1cards.dequeue()
    val p2card = p2cards.dequeue()

    val winner =
      if(p1cards.size >= p1card && p2cards.size >= p2card) playRecursiveGame(p1card, p2card)
      else if (p1card > p2card) 1 else 2

    (if(winner == 1) p1cards else p2cards)
      .enqueueAll(if(winner == 1) List(p1card, p2card) else List(p2card, p1card))
  }

  def playRecursiveGame(p1card: Int, p2card: Int): Int =
    new Aoc22Recursive(p1cards.take(p1card), p2cards.take(p2card)).playGame()
}

class Aoc22(var p1cards: mutable.Queue[Int] = mutable.Queue[Int](),
            var p2cards: mutable.Queue[Int] = mutable.Queue[Int]()) {

  def winningPlayerScore(): Int = (if(p1cards.nonEmpty) p1cards else p2cards)
    .toList.reverse.zipWithIndex.map(card => card._1 * (card._2 + 1)).sum

  def playGame(): Int = {
    while(gameIsGoingOn) playCard()
    if(p1cards.nonEmpty) 1 else 2
  }

  def playCard(): Unit = {
    val p1card = p1cards.dequeue()
    val p2card = p2cards.dequeue()

    val winningQueue = if(p1card > p2card) p1cards else p2cards

    List(p1card, p2card).sorted.reverse.foreach(card => winningQueue.enqueue(card))
  }

  def gameIsGoingOn: Boolean = p1cards.nonEmpty && p2cards.nonEmpty

  def parse(input: List[String]): Unit = {
    val split = input.mkString("--").split("----")
    parsePlayer(split(0).split("--").drop(1), p1cards)
    parsePlayer(split(1).split("--").drop(1), p2cards)
  }

  def parsePlayer(cards: Array[String], queue: mutable.Queue[Int]): Unit =
    cards.map(_.toInt).foreach(card => queue.enqueue(card))
}

class Aoc22Tests extends TestBase {
  test("Ex 1") {
    Aoc22.playClassicGame(readStrings("Aoc22example.txt")) shouldBe 306
  }

  test("Puzzle 1") {
    Aoc22.playClassicGame(readStrings("Aoc22.txt")) shouldBe 30138
  }

  test("Ex 2") {
    Aoc22.playRecursiveGame(readStrings("Aoc22example.txt")) shouldBe 291
  }

  test("Puzzle 2") {
    Aoc22.playRecursiveGame(readStrings("Aoc22.txt")) shouldBe 31587
  }
}