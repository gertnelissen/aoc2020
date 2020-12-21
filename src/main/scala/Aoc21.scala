import Util._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scala.collection.mutable

class Aoc21(input: List[String]) {
  case class Recipe(ingredients: Array[String], allergen: Array[String]) {
    def asString(): String = s"${ingredients.mkString(" ")} (${allergen.mkString(" ")})"
  }

  val recipes: List[Recipe] = parseInput(input)

  def countPossiblySafeIngredients(): Int = {
    val allergensWithRecipes = getAllergensWithRecipes(recipes)
    val possibleAllergenSources = determinePossilbeAllergenSources(allergensWithRecipes).flatMap(_._2).toList
    val sourcesThatMayBeAllergenFree = allSources().filterNot(source => possibleAllergenSources.contains(source))
    recipes.flatMap(_.ingredients).count(sourcesThatMayBeAllergenFree.contains)
  }

  def allergenSources(): String = {
    val allergensWithRecipes = getAllergensWithRecipes(recipes)
    val possibleAllergenSources = determinePossilbeAllergenSources(allergensWithRecipes)
    val allergenSources = determineAllergenSources(possibleAllergenSources)
    allergenSources.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  def determineAllergenSources(possibleAllergenSources: Map[String, List[String]]): Map[String, String] = {
    val unassignedMap = mutable.HashMap(possibleAllergenSources.toSeq: _*)
    val assignedMap = mutable.Map[String, String]()

    do {
      unassignedMap.filter(_._2.size == 1).foreach(clearMapping => {
        assignedMap.update(clearMapping._1, clearMapping._2.head)
        unassignedMap.remove(clearMapping._1)
        unassignedMap.filter(_._2.contains(clearMapping._2.head)).foreach(mappingToAdjust =>
          unassignedMap.update(mappingToAdjust._1, mappingToAdjust._2.filterNot(_ == clearMapping._2.head)))
      })
    } while(unassignedMap.nonEmpty)

    assignedMap.toMap
  }

  def allSources(): List[String] = recipes.flatMap(_.ingredients).distinct

  def allAllergens(): List[String] = recipes.flatMap(_.allergen).distinct

  def determinePossilbeAllergenSources(allergensWithRecipes: Map[String, List[Recipe]]): Map[String, List[String]] = {
    allergensWithRecipes.map(allergen => allergen._1 -> sourcesPresentInAllRecipes(allergen._2))
  }

  def sourcesPresentInAllRecipes(recipes: List[Recipe]): List[String] = {
    recipes.map(_.ingredients).reduce((a, b) => a.intersect(b)).toList
  }

  def getAllergensWithRecipes(recipes: List[Recipe]): Map[String, List[Recipe]] = {
    allAllergens().map(allergen => allergen -> recipes.filter(_.allergen.contains(allergen))).toMap
  }

  def parseInput(input: List[String]): List[Recipe] = input.map(parseRecipe)

  def parseRecipe(input: String): Recipe = {
    val split = input.stripSuffix(")").split("\\(contains ")
    Recipe(split(0).trim.split(" "), split(1).split(", "))
  }
}

class Aoc21Tests extends TestBase {
  test("Ex 1") {
    val input = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                  |trh fvjkl sbzzf mxmxvkd (contains dairy)
                  |sqjhc fvjkl (contains soy)
                  |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin
    new Aoc21(textToStrings(input)).countPossiblySafeIngredients() shouldBe 5
  }

  test("Puzzle 1") {
    new Aoc21(readStrings("Aoc21.txt")).countPossiblySafeIngredients() shouldBe 2595
  }

  test("Ex 2") {
    val input = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                  |trh fvjkl sbzzf mxmxvkd (contains dairy)
                  |sqjhc fvjkl (contains soy)
                  |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin
    new Aoc21(textToStrings(input)).allergenSources() shouldBe "mxmxvkd,sqjhc,fvjkl"
  }

  test("Puzzle 2") {
    new Aoc21(readStrings("Aoc21.txt")).allergenSources() shouldBe "thvm,jmdg,qrsczjv,hlmvqh,zmb,mrfxh,ckqq,zrgzf"
  }
}