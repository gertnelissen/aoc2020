case class Pair(x: Int, y: Int) {
  def add(other: Pair): Pair = {
    Pair(
      this.x + other.x,
      this.y + other.y
    )
  }

  def subtract(other: Pair): Pair = {
    Pair(
      this.x - other.x,
      this.y - other.y
    )
  }
}