import scala.io.Source.fromURL

object Util {
  // List of X to List of Y
  def stringsToInts(input: List[String]): List[Int] = {
    input.map(stringToInt)
  }

  def stringToInt(input: String): Int = {
    input.trim.toInt
  }

  // Text block to ...
  def textToStrings(input: String): List[String] = {
    input.trim.split("\n").toList
  }

  def textToInts(input: String): List[Int] = {
    (textToStrings _ andThen stringsToInts)(input)
  }

  // Read file to List of ...
  def readStrings(path: String): List[String] = {
    val file = fromURL(getClass.getResource(path))
    try file.getLines().toList finally file.close()
  }

  def readInts(path: String): List[Int] = {
    (readStrings _ andThen stringsToInts)(path)
  }
}
