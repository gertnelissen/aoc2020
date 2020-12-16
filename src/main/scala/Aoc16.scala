import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

object Aoc16 {
  def solve(input: List[String]): Int = new Aoc16(input).invalidTicketSum()

  def validate(input: List[String]): Long = new Aoc16(input).validateDepartureSum()
}

class Aoc16(input: List[String]) {
  case class Rule(name: String, aFilter: Pair, bFilter: Pair)

  val rules: List[Rule] = parseRules(input)
  val yourTicket: List[Int] = parseYourTicket(input)
  val nearbyTickets: List[List[Int]] = parseNearbyTickets(input)

  var assignedRules: mutable.Map[Int, Rule] = mutable.Map[Int, Rule]()

  def validateDepartureSum(): Long = {
    determineRulePositions(rules, nearbyTickets.filter(ticket => inValidFields(ticket).isEmpty))
    assignedRules.filter(_._2.name.startsWith("departure")).keys.map(yourTicket).map(_.toLong).product
  }

  def determineRulePositions(rules: List[Rule], tickets: List[List[Int]]): Unit = {
    while(assignedRules.size < rules.size)
      tryToDetermineNextRulePosition(rules, tickets)
  }

  def tryToDetermineNextRulePosition(rules: List[Rule], tickets: List[List[Int]]): Unit = {
    rules
      .filterNot(assignedRules.values.toList.contains)
      .map(rule => (rule, determinePossibleRulePositions(rule, tickets)))
      .filter(_._2.size == 1)
      .foreach(determinedRule => assignedRules.update(determinedRule._2.head, determinedRule._1))
  }

  def determinePossibleRulePositions(rule: Rule, tickets: List[List[Int]]): List[Int] = {
    tickets.head.indices
      .filter(position => positionIsValidForRule(rule, tickets, position))
      .filterNot(assignedRules.contains).toList
  }

  def positionIsValidForRule(rule: Rule, tickets: List[List[Int]], position: Int): Boolean = {
    tickets.forall(ticket => ruleMatchesField(rule, ticket(position)))
  }

  def invalidTicketSum(): Int = nearbyTickets.flatMap(inValidFields).sum

  def inValidFields(ticket: List[Int]): List[Int] = ticket.filter(invalidField)

  def invalidField(field: Int): Boolean = rules.forall(rule => !ruleMatchesField(rule, field))

  def ruleMatchesField(rule: Rule, field: Int): Boolean = {
    ruleMatchesFilter(rule.aFilter, field) || ruleMatchesFilter(rule.bFilter, field)
  }

  def ruleMatchesFilter(filter: Pair, field: Int): Boolean = {
    field >= filter.x && field <= filter.y
  }

  def parseRules(input: List[String]): List[Rule] = {
    val asHugeString = input.mkString("--")
    asHugeString.substring(0, asHugeString.indexOf("---")).split("--").filterNot(_.isEmpty).map(parseRule).toList
  }

  def parseRule(input: String): Rule = {
    val split = input.split(':')
    val filterSplit = split(1).trim.split(" or ").flatMap(_.split('-')).map(_.toInt)
    Rule(split(0), Pair(filterSplit(0), filterSplit(1)), Pair(filterSplit(2), filterSplit(3)))
  }

  def parseYourTicket(input: List[String]): List[Int] = {
    val s = input.mkString("--")
    s.substring(s.indexOf("your ticket:")).split("--").drop(1).filterNot(_.isEmpty).take(1).map(parseTicket).head
  }

  def parseNearbyTickets(input: List[String]): List[List[Int]] = {
    val s = input.mkString("--")
    s.substring(s.indexOf("nearby tickets:")).split("--").drop(1).filterNot(_.isEmpty).map(parseTicket).toList
  }

  def parseTicket(input: String): List[Int] = input.split(',').map(_.toInt).toList
}

class Aoc16Tests extends TestBase {
  test("Ex 1") {
    Aoc16.solve(textToStrings("""class: 1-3 or 5-7
                  |row: 6-11 or 33-44
                  |seat: 13-40 or 45-50
                  |
                  |your ticket:
                  |7,1,14
                  |
                  |nearby tickets:
                  |7,3,47
                  |40,4,50
                  |55,2,20
                  |38,6,12""".stripMargin)) shouldBe 71
  }

  test("Puzzle 1") {
    Aoc16.solve(readStrings("Aoc16.txt")) shouldBe 24110
  }

  test("Ex 2") {
    Aoc16.validate(textToStrings("""class: 0-1 or 4-19
                                |departureClass: 0-5 or 8-19
                                |departureSeat: 0-13 or 16-19
                                |
                                |your ticket:
                                |11,12,13
                                |
                                |nearby tickets:
                                |3,9,18
                                |15,1,5
                                |5,14,9""".stripMargin)) shouldBe 143
  }

  test("Puzzle 2") {
    Aoc16.validate(readStrings("Aoc16.txt")) shouldBe 6766503490793L
  }
}