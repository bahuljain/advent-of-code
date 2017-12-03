package aoc2017

import scala.io.Source

object Day2 extends App {
  val in = Source.fromFile("src/aoc2017/day2.input.txt").getLines.map(_.split("\\s+").map(_.toInt).toList).toList

  val p1 = in.map(row => row.max - row.min).sum

  def evenlyDivisible(a: List[Int]): Int = if (a.last % a.head == 0) a.last / a.head else 0

  val p2 = in.flatMap(_.combinations(2).map(_.sorted).map(evenlyDivisible)).sum

  println(p1)
  println(p2)
}
