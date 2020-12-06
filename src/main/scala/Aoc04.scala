import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Aoc04 {
  def countValidPassports(input: List[String], checkContentValidity: Boolean = false): Int = {
    val solver = new Aoc04(checkContentValidity)
    val passports = solver.parse(input)
    passports.foreach(println)
    solver.countValidPassports(passports)
  }
}

class Aoc04(checkContentValidity: Boolean) {
  case class Passport(properties: Map[String, String])

  val mandatoryProperties = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def parse(input: List[String]): List[Passport] = {
    input.mkString("-").split("--").map(parsePassport).toList
  }

  def parsePassport(input: String): Passport = {
    Passport(input.split(Array(' ', '\n', '-')).map(_.trim).map(property => {
      val split = property.split(':')
      split(0) -> split(1)
    }).toMap)
  }

  def countValidPassports(passports: List[Passport]): Int = {
    passports.count(isValidPassport)
  }

  def isValidPassport(passport: Passport): Boolean = {
    mandatoryProperties.forall(property => {
      passport.properties.contains(property) &&
        (!checkContentValidity || isValidProperty(property, passport.properties(property)))
    })
  }

  def isValidProperty(property: String, value: String): Boolean = {
    property match {
      case "byr" => value.length == 4 && value.toInt >= 1920 && value.toInt <= 2002
      case "iyr" => value.length == 4 && value.toInt >= 2010 && value.toInt <= 2020
      case "eyr" => value.length == 4 && value.toInt >= 2020 && value.toInt <= 2030
      case "hgt" => isHeight(value)
      case "hcl" => value.charAt(0) == '#' && value.length == 7 && value.substring(1).forall(isHex)
      case "ecl" => List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
      case "pid" => value.length == 9 && value.forall(_.isDigit)
      case _ => true
    }
  }

  def isHeight(str: String): Boolean = {
    if(str.endsWith("cm")) {
      val height = str.stripSuffix("cm").toInt
      height >= 150 && height <= 193
    } else if(str.endsWith("in")) {
      val height = str.stripSuffix("in").toInt
      height >= 59 && height <= 76
    } else
      false
  }

  def isHex(c: Char): Boolean = {
    c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  }
}

class Aoc04Tests extends TestBase {
  test("Ex 1") {
    val input = textToStrings(
      """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
        |byr:1937 iyr:2017 cid:147 hgt:183cm
        |
        |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
        |hcl:#cfa07d byr:1929
        |
        |hcl:#ae17e1 iyr:2013
        |eyr:2024
        |ecl:brn pid:760753108 byr:1931
        |hgt:179cm
        |
        |hcl:#cfa07d eyr:2025 pid:166559648
        |iyr:2011 ecl:brn hgt:59in""".stripMargin)

    Aoc04.countValidPassports(input) shouldBe 2
  }

  test("Puzzle 1") {
    Aoc04.countValidPassports(readStrings("Aoc04.txt")) shouldBe 202
  }

  /*test("Ex 2") {
    val input = textToStrings(
      """..##.......
        |#...#...#..
        |.#....#..#.
        |..#.#...#.#
        |.#...##..#.
        |..#.##.....
        |.#.#.#....#
        |.#........#
        |#.##...#...
        |#...##....#
        |.#..#...#.#""".stripMargin)

    val slopes = List(Pair(1, 1), Pair(3, 1), Pair(5, 1), Pair(7, 1), Pair(1, 2))
    slopes.map(slope => Aoc03.countEncounteredTreesWithSlope(input, slope)).product shouldBe 336
  }*/

  test("Puzzle 2") {
    Aoc04.countValidPassports(readStrings("Aoc04.txt"), true) shouldBe 202
  }
}