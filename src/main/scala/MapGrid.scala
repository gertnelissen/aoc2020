import scala.collection.mutable
import scala.math.abs

class MapGrid {
  var map: mutable.Map[Pair, Char] = mutable.Map[Pair, Char]()

  def parse(input: List[String]): Unit = {
    for(y <- input.indices)
      for(x <- 0 until input(y).length)
        map.put(Pair(x, y), input(y).charAt(x))
  }

  def print(): Unit = {
    for(y <- minY to maxY)
      println((minX to maxX).map(getField(_, y)).mkString)
  }

  def getField(x: Int, y: Int): Char = getField(Pair(x, y))

  def getField(pair: Pair): Char = map.getOrElse(pair, '?')

  def setField(x: Int, y: Int, value: Char): Unit = setField(Pair(x, y), value)

  def setField(pair: Pair, value: Char): Unit = {
    if(map.isDefinedAt(pair))
      map.update(pair, value)
    else
      map.put(pair, value)
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
