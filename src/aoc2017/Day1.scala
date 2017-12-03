package aoc2017

object Day1 extends App {
  val in = scala.io.Source.fromFile("src/aoc2017/day1.input.txt").mkString

  def adder(off: Int) = (0 until in.length).filter(i => in(i) == in((i + off) % in.length)).map(in(_) - '0').sum

  println(s"part 1 - ${adder(1)}\npart 2 - ${adder(in.length / 2)}")
}