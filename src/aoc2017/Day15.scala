package aoc2017

case class Generators(A: Long = 703, B: Long = 516) {
  private def next(factor: Int, value: Long): Long = (value * factor) % 2147483647

  def next: Generators = Generators(next(16807, A), next(48271, B))

  def isMatching: Boolean = (A % 131072) equals (B % 131072)
}

object Day15 extends App {
  val count = Stream.iterate(Generators(), 40000000)(_.next).filter(_.isMatching).size

  println(count)
}
