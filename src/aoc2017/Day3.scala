package aoc2017

import scala.collection.mutable
import scala.util.Try

object Day3 extends App {
  val in = 312051


  def solution1(x: Int): Int = {
    def box(i: Int): Int = Math.pow(2 * i + 1, 2).toInt
    val r = Stream.from(0).find(box(_) >= x).get
    val d = Try(Stream.continually((r until 0 by -1) ++ (0 until r)).flatten.apply(box(r) - x)).getOrElse(0)
    r + d
  }

  def solution2(x: Int): Int = {
    val grid = mutable.Map[(Int, Int), Int]((0, 0) -> 1, (1, 0) -> 1)
    var (r, step, dir) = (1, 1, (0, 1))
    def g(loc: (Int, Int)) = grid.getOrElse(loc, 0)
    def getSum(loc: (Int, Int)) = {
      val (u, v) = loc
      val sum = g((u + 1, v)) + g((u + 1, v + 1)) + g((u, v + 1)) + g((u - 1, v + 1)) + g((u - 1, v)) +
        g((u - 1, v - 1)) + g((u, v - 1)) + g((u + 1, v - 1))
      grid.put((u, v), sum)
      sum
    }
    def next(prev: (Int, Int)): (Int, Int) = {
      val (u, v) = prev
      if (u == r && v == -r) {
        r += 1
        step = 2 * r - 1
        (u + 1, v)
      } else {
        val nxt = (u + dir._1, v + dir._2)
        if (step == 1) {
          step = 2 * r
          dir = (- dir._2, dir._1)
        } else {
          step -= 1
        }
        nxt
      }
    }

    Stream.iterate((1, 0))(next).map(getSum).find(_ > x).get
  }

  println(solution1(in))
  println(solution2(in))
}
