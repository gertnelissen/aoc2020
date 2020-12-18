import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

object Aoc18 {
  def solve(input: String, plusPrecedence: Boolean): Long =
    new Aoc18(input, plusPrecedence).solve()

  def solve(input: List[String], plusPrecedence: Boolean): Long =
    input.map(problem => solve(problem, plusPrecedence)).sum
}

class Aoc18(inputText: String, plusPrecedence: Boolean = false) {
  def solve(): Long = {
    var problem = parse(inputText).toList

    while(problem.length > 1)
      problem = reduceProblem(problem)

    problem.head.toLong
  }

  def reduceProblem(problem: List[String]): List[String] = {
    if(problem.contains(")"))
      reduceParentheses(problem)
    else if(plusPrecedence && problem.contains("+"))
      reducePlusOperators(problem)
    else
      reduceOperators(problem)
  }

  def reduceParentheses(problem: List[String]): List[String] = {
    val indexOfClosingParentheses = problem.indexWhere(_ == ")")
    val indexOfOpeningParentheses = problem.lastIndexWhere(_ == "(", indexOfClosingParentheses)

    problem.slice(0, indexOfOpeningParentheses) :::
      reduceProblem(problem.slice(indexOfOpeningParentheses + 1, indexOfClosingParentheses)) :::
      problem.slice(indexOfClosingParentheses + 1, problem.length)
  }

  def reducePlusOperators(problem: List[String]): List[String] = {
    val indexOfPlus = problem.indexWhere(_ == "+")

    reduceProblem(
      problem.slice(0, indexOfPlus - 1) :::
      reduceOperators(problem.slice(indexOfPlus - 1, indexOfPlus + 2)) :::
      problem.slice(indexOfPlus + 2, problem.length)
    )
  }

  def reduceOperators(problem: List[String]): List[String] = {
    var total = problem.head.toLong
    var operator: String = null

    problem.drop(1).foreach(item =>
      if(isOperator(item))
        operator = item
      else
        total = reduceOperator(total, operator, item.toLong)
    )

    List(total.toString)
  }

  def reduceOperator(a: Long, operator: String, b: Long): Long = {
    operator match {
      case "*" => a * b
      case "+" => a + b
      case _ => throw new Exception(s"Operator $operator not supported")
    }
  }

  def isOperator(s: String): Boolean = s == "+" || s == "*"

  def parse(input: String): Array[String] = {
    input
      .replace("(", " ( ").replace(")", " ) ")
      .split(" ")
      .filterNot(_.isEmpty)
  }
}

class Aoc18Tests extends TestBase {
  test("Ex 1") {
    Aoc18.solve("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", false) shouldBe 13632
  }

  test("Puzzle 1") {
    Aoc18.solve(readStrings("Aoc18.txt"), false) shouldBe 1451467526514L
  }

  test("Ex 2") {
    Aoc18.solve("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", true) shouldBe 23340
  }

  test("Puzzle 2") {
    Aoc18.solve(readStrings("Aoc18.txt"), true) shouldBe 224973686321527L
  }
}