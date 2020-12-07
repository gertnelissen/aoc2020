import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc07 {
  def reachableBy(input: List[String], target: String): Int = {
    val solver = new Aoc07()
    solver.parse(input)
    solver.reaching(solver.parseBag(target)).size
  }

  def bagsIn(input: List[String], target: String): Int = {
    val solver = new Aoc07()
    solver.parse(input)
    solver.bagsIn(solver.parseBag(target))
  }
}

class Aoc07() {
  case class Bag(style: String, color: String)
  case class Constraint(count: Int, bag: Bag)

  var rules: Map[Bag, List[Constraint]] = null

  def reaching(bag: Bag): Set[Bag] = {
    val outerBags = bagsAbleToHold(bag)
    outerBags.union(outerBags.flatMap(reaching))
  }

  def bagsAbleToHold(bag: Bag): Set[Bag] = {
    rules.filter(_._2.map(_.bag).exists(isSame(_, bag))).keys.toSet
  }

  def isSame(a: Bag, b: Bag): Boolean = {
    a.style == b.style && a.color == b.color
  }

  def bagsIn(bag: Bag): Int = {
    rules(bag).map(rule => rule.count * (1 + bagsIn(rule.bag))).sum
  }

  def parse(input: List[String]): Unit = {
    rules = input.map(parseRule).toMap
  }

  def parseRule(input: String): (Bag, List[Constraint]) = {
    val split = input.split("contain")
    parseBag(split(0)) -> parseConstraints(split(1))
  }

  def parseBag(input: String): Bag = {
    val split = input.trim.stripSuffix("bags").stripSuffix("bag").trim.split(' ')
    Bag(split(0).trim, split(1).trim)
  }

  def parseConstraints(input: String): List[Constraint] = {
    if(input.trim == "no other bags.")
      return List()
    input.trim.stripSuffix(".").split(',').map(parseConstraint).toList
  }

  def parseConstraint(input: String): Constraint = {
    val split = input.trim.split(' ')
    Constraint(split(0).trim.toInt, parseBag(split.drop(1).mkString(" ")))
  }

  def printRules(): Unit = {
    rules.foreach(rule => println("%s -> %s".format(rule._1, rule._2)))
  }
}

class Aoc07Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """light red bags contain 1 bright white bag, 2 muted yellow bags.
        |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        |bright white bags contain 1 shiny gold bag.
        |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        |faded blue bags contain no other bags.
        |dotted black bags contain no other bags.""".stripMargin)

    Aoc07.reachableBy(input, "shiny gold") shouldBe 4
  }

  test("Puzzle 1") {
    Aoc07.reachableBy(readStrings("Aoc07.txt"), "shiny gold") shouldBe 370
  }

  test("Ex 2") {
    val input = textToStrings(
      """light red bags contain 1 bright white bag, 2 muted yellow bags.
        |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        |bright white bags contain 1 shiny gold bag.
        |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        |faded blue bags contain no other bags.
        |dotted black bags contain no other bags.""".stripMargin)

    Aoc07.bagsIn(input, "shiny gold") shouldBe 32
  }

  test("Ex 3") {
    val input = textToStrings(
      """shiny gold bags contain 2 dark red bags.
        |  dark red bags contain 2 dark orange bags.
        |  dark orange bags contain 2 dark yellow bags.
        |  dark yellow bags contain 2 dark green bags.
        |  dark green bags contain 2 dark blue bags.
        |  dark blue bags contain 2 dark violet bags.
        |  dark violet bags contain no other bags.""".stripMargin)

    Aoc07.bagsIn(input, "shiny gold") shouldBe 126
  }

  test("Puzzle 2") {
    Aoc07.bagsIn(readStrings("Aoc07.txt"), "shiny gold") shouldBe 29547
  }
}