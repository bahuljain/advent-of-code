package aoc2017

import scala.io.Source

object Day4 extends App {
  val in = Source.fromFile("src/aoc2017/day4.input.txt").getLines.map(_.split("\\s+").toList).toList

  val p1 = in.count(pass => pass.distinct == pass)
  val p2 = in.count(_.combinations(2).forall(_.map(_.toSet).distinct.size == 2))

  println(p1, p2)
}
