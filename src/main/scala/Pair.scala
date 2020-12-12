case class Pair(x: Int, y: Int) {
  def add(other: Pair): Pair = add(other.x, other.y)

  def add(other: (Int, Int)): Pair = {
    Pair(
      this.x + other._1,
      this.y + other._2
    )
  }

  def subtract(other: Pair): Pair = {
    Pair(
      this.x - other.x,
      this.y - other.y
    )
  }
}