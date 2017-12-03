package aoc2017

import scala.collection.mutable
import scala.util.Try

object Day3 extends App {
  val in = 312051


  def solution1(x: Int): Int = {
    def box(i: Int): Int = Math.pow(2 * i + 1, 2).toInt
    val r = Stream.from(0).find(box(_) >= x).get
    val d = Stream.continually((r to 0 by -1) ++ (1 until r)).flatten.apply(box(r) - x)
    r + d
  }

  def solution2(a: Int): Int = {
    type Vec = (Int, Int)
    val grid = mutable.Map[Vec, Int]((0, 0) -> 1)
    def g(pos: Vec) = grid.getOrElse(pos, 0)
    def getSum(pos: Vec) = {
      val (x, y) = pos
      val sum = g((x + 1, y)) + g((x + 1, y + 1)) + g((x, y + 1)) + g((x - 1, y + 1)) + g((x - 1, y)) +
        g((x - 1, y - 1)) + g((x, y - 1)) + g((x + 1, y - 1)) + g((x, y))
      grid.put((x, y), sum)
      sum
    }
    var (r, dir) = (0, (0, 1))
    def out(loc: Vec, dir: Vec, r: Int) = Math.pow(loc._1 + dir._1, 2) + Math.pow(loc._2 + dir._2, 2) > 2 * r * r
    def spiralNext(prev: Vec): Vec = {
      val (x, y) = prev
      if (x == r && y == -r) {
        r += 1
        (x + 1, y)
      } else {
        val nxt = (x + dir._1, y + dir._2)
        if (out(nxt, dir, r))
          dir = (- dir._2, dir._1)
        nxt
      }
    }

    Stream.iterate((0, 0))(spiralNext).map(getSum).find(_ > a).get
  }

  println(solution1(in))
  println(solution2(in))
}
