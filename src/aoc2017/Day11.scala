package aoc2017

object Day11 extends App {
  val in = scala.io.Source.fromFile("src/aoc2017/day11.input.txt").mkString.split(",").toIterator

  case class Hex(x: Int, y: Int) {
    def move: Hex = scala.util.Try(in.next) map {
      case "n" => Hex(x, y + 2)
      case "nw" => Hex(x - 1, y + 1)
      case "sw" => Hex(x - 1, y - 1)
      case "s" => Hex(x, y - 2)
      case "se" => Hex(x + 1, y - 1)
      case "ne" => Hex(x + 1, y + 1)
    } getOrElse null

    val distance: Int = {
      val diff = Math.abs(Math.abs(x) - Math.abs(y))
      Math.min(Math.abs(x), Math.abs(y)) + (if (Math.abs(x) > Math.abs(y)) diff else diff / 2)
    }
  }

  val path = Stream.iterate(Hex(0, 0))(_.move).takeWhile(_ != null).map(_.distance)

  println(s"part 1 - ${path.last}\npart 2 - ${path.max}")
}
