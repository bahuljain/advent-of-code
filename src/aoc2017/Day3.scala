package aoc2017

object Day3 extends App {
  val in = 312051


  def solution1(x: Int): Int = {
    def box(i: Int): Int = Math.pow(2 * i + 1, 2).toInt

    val r = Stream.from(0).find(box(_) >= x).get
    val d = Stream.continually((r to 0 by -1) ++ (1 until r)).flatten.apply(box(r) - x)
    r + d
  }

  case class Vec(x: Int, y: Int) {
    val r: Int = Math.max(Math.abs(x), Math.abs(y))

    val atBottomRightEdge: Boolean = x == r && y == -r

    def neighbors = List(Vec(x + 1, y), Vec(x + 1, y + 1), Vec(x, y + 1), Vec(x - 1, y + 1), Vec(x - 1, y),
      Vec(x - 1, y - 1), Vec(x, y - 1), Vec(x + 1, y - 1), this)

    def isOutOfBoundary(dir: Vec): Boolean = Math.pow(x + dir.x, 2) + Math.pow(y + dir.y, 2) > 2 * r * r

    def move(dir: Vec): Vec = Vec(x + dir.x, y + dir.y)
  }

  def solution2(a: Int): Int = {
    var (dir, grid) = (Vec(0, 1), Map[Vec, Int](Vec(0, 0) -> 1))

    def getSum(pos: Vec) = {
      val sum = pos.neighbors.map(grid.getOrElse(_, 0)).sum
      grid = grid + (pos -> sum)
      sum
    }

    def spiralNext(prev: Vec): Vec = {
      if (prev.atBottomRightEdge)
        prev.move(Vec(1, 0))
      else {
        val nxt = prev.move(dir)
        if (nxt.isOutOfBoundary(dir))
          dir = Vec(-dir.y, dir.x)
        nxt
      }
    }

    Stream.iterate(Vec(0, 0))(spiralNext).map(getSum).find(_ > a).get
  }

  println(solution1(in))
  println(solution2(in))
}