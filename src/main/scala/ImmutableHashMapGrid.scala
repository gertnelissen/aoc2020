
class ImmutableHashMapGrid {
  var map: Map[Pair, Char] = Map[Pair, Char]()

  def parse(input: List[String]): Unit = {
    map = input.indices.flatMap(y => {
      (0 until input(y).length).map(x => {
        Pair(x, y) -> input(y).charAt(x)
      })
    }).toMap
  }

  def print(): Unit = {
    for(y <- minY to maxY)
      println((minX to maxX).map(getField(_, y)).mkString)
  }

  def getField(pos: (Int, Int)): Char = getField(Pair(pos._1, pos._2))

  def getField(x: Int, y: Int): Char = getField(Pair(x, y))

  def getField(pair: Pair): Char = map.getOrElse(pair, '?')

  def countsPerCharacter(): Map[Char, Int] = {
    map.groupBy(_._2).view.mapValues(_.size).toMap
  }

  def minX: Int = map.keys.map(_.x).min
  def maxX: Int = map.keys.map(_.x).max

  def minY: Int = map.keys.map(_.y).min
  def maxY: Int = map.keys.map(_.y).max

  def sizeX: Int = (maxX - minX) + 1
  def sizeY: Int = (maxY - minY) + 1

  def minCoordinate: Pair = Pair(minX, minY)
  def maxCoordinate: Pair = Pair(maxX, maxY)
}
