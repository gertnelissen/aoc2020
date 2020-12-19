import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc19 {
  def solve(input: List[String], updates: List[String] = List()): Int = {
    val solver = new Aoc19(input)
    solver.applyUpdates(updates)
    solver.getNumMatchingWords
  }
}

trait Rule {
  def toString: String
  def accepts(word: String, rules: Map[Int, Rule]): Array[(Boolean, String)]
}

class SequentialRule(ruleNumbers: Array[Int]) extends Rule {
  override def toString: String = ruleNumbers.map(_.toString).mkString(" ")

  override def accepts(word: String, rules: Map[Int, Rule]): Array[(Boolean, String)] = {
    val words = Iterator.iterate((ruleNumbers, List(word)))(iteration => {
      (iteration._1.drop(1),
        iteration._2.flatMap(word => rules(iteration._1.head).accepts(word, rules).filter(_._1).map(_._2)))
    }).find(_._1.isEmpty).get._2

    if(words.isEmpty) Array((false, word))
    else words.map(word => (true, word)).toArray
  }
}

class LiteralRule(literal: String) extends Rule{
  override def toString: String = s"<$literal>"

  override def accepts(word: String, rules: Map[Int, Rule]): Array[(Boolean, String)] = {
    if(word.startsWith(literal)) Array((true, word.stripPrefix(literal)))
    else Array((false, word))
  }
}

class OrRule(rules: Array[Rule]) extends Rule {
  override def toString: String = rules.map(_.toString).mkString(" | ")

  override def accepts(word: String, rules: Map[Int, Rule]): Array[(Boolean, String)] = {
    val matchingRules = this.rules.flatMap(rule => rule.accepts(word, rules)).filter(_._1 == true)

    if(matchingRules.isEmpty) Array((false, word))
    else matchingRules
  }
}

class Matcher(input: List[String]) {
  var rules: Map[Int, Rule] = createRules(input)

  def applyUpdates(updates: List[String]): Unit =
    updates.map(createRule).foreach(rule => rules = rules.updated(rule._1, rule._2))

  def createRules(input: List[String]): Map[Int, Rule] = input.map(createRule).toMap

  def createRule(input: String): (Int, Rule) = {
    val split = input.split(": ")
    (split(0).toInt, createRuleFromPart(split(1)))
  }

  def createRuleFromPart(s: String): Rule = {
    if(s.contains('|')) new OrRule(s.split('|').map(_.trim).map(createRuleFromPart))
    else if(s.contains("\"")) new LiteralRule(s.trim.replace("\"", ""))
    else new SequentialRule(s.split(' ').filterNot(_.isEmpty).map(_.toInt))
  }

  def accepts(word: String): Boolean = rules(0).accepts(word, rules).exists(outcome => outcome._1 && outcome._2.isEmpty)
}

class Aoc19(input: List[String]) {
  var matcher: Matcher = new Matcher(input.slice(0, input.indexWhere(_.isEmpty)))
  var words: List[String] = input.slice(input.indexWhere(_.isEmpty) + 1, input.length)

  def getNumMatchingWords: Int = words.count(matcher.accepts)

  def applyUpdates(updates: List[String]): Unit = matcher.applyUpdates(updates)
}

class Aoc19Tests extends TestBase {
  test("Ex 1") {
    val input = """0: 4 1 5
                  |1: 2 3 | 3 2
                  |2: 4 4 | 5 5
                  |3: 4 5 | 5 4
                  |4: "a"
                  |5: "b"
                  |
                  |ababbb
                  |bababa
                  |abbbab
                  |aaabbb
                  |aaaabbb""".stripMargin
    Aoc19.solve(textToStrings(input)) shouldBe 2
  }

  test("Puzzle 1") {
    Aoc19.solve(readStrings("Aoc19.txt")) shouldBe 132
  }

  test("Ex 2") {
    val input = """42: 9 14 | 10 1
                  |9: 14 27 | 1 26
                  |10: 23 14 | 28 1
                  |1: "a"
                  |11: 42 31
                  |5: 1 14 | 15 1
                  |19: 14 1 | 14 14
                  |12: 24 14 | 19 1
                  |16: 15 1 | 14 14
                  |31: 14 17 | 1 13
                  |6: 14 14 | 1 14
                  |2: 1 24 | 14 4
                  |0: 8 11
                  |13: 14 3 | 1 12
                  |15: 1 | 14
                  |17: 14 2 | 1 7
                  |23: 25 1 | 22 14
                  |28: 16 1
                  |4: 1 1
                  |20: 14 14 | 1 15
                  |3: 5 14 | 16 1
                  |27: 1 6 | 14 18
                  |14: "b"
                  |21: 14 1 | 1 14
                  |25: 1 1 | 1 14
                  |22: 14 14
                  |8: 42
                  |26: 14 22 | 1 20
                  |18: 15 15
                  |7: 14 5 | 1 21
                  |24: 14 1
                  |
                  |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
                  |bbabbbbaabaabba
                  |babbbbaabbbbbabbbbbbaabaaabaaa
                  |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
                  |bbbbbbbaaaabbbbaaabbabaaa
                  |bbbababbbbaaaaaaaabbababaaababaabab
                  |ababaaaaaabaaab
                  |ababaaaaabbbaba
                  |baabbaaaabbaaaababbaababb
                  |abbbbabbbbaaaababbbbbbaaaababb
                  |aaaaabbaabaaaaababaa
                  |aaaabbaaaabbaaa
                  |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
                  |babaaabbbaaabaababbaabababaaab
                  |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin
    val updates = """8: 42 | 42 8
                    |11: 42 31 | 42 11 31""".stripMargin
    Aoc19.solve(textToStrings(input), textToStrings(updates)) shouldBe 12
  }

  test("Puzzle 2") {
    val input = readStrings("Aoc19.txt")
    val updates = """8: 42 | 42 8
                    |11: 42 31 | 42 11 31""".stripMargin
    Aoc19.solve(input, textToStrings(updates)) shouldBe 306
  }
}