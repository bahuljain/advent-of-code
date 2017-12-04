package aoc2017

import scala.io.Source

object Day4 extends App {
  val in = Source.fromFile("src/aoc2017/day4.input.txt").getLines.map(_.split("\\s+").toList).toList
  val p1 = in.count(pass => pass.distinct == pass)
  val p2 = in.count(pass => pass.groupBy(_.sorted).size == pass.length)
  println(s"part 1 - $p1\npart 2 - $p2")
}
