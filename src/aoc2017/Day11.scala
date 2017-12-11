package aoc2017

object Day11 extends App {
  val in = scala.io.Source.fromFile("src/aoc2017/day11.input.txt").mkString.split(",")

  case class Hex(x: Int, y: Int) {
    def move(direction: String): Hex = direction match {
      case "n" => Hex(x, y + 2)
      case "nw" => Hex(x - 1, y + 1)
      case "sw" => Hex(x - 1, y - 1)
      case "s" => Hex(x, y - 2)
      case "se" => Hex(x + 1, y - 1)
      case "ne" => Hex(x + 1, y + 1)
      case _ => throw new Exception(s"Invalid direction - $direction")
    }

    val distance: Int = {
      val diff = Math.abs(Math.abs(x) - Math.abs(y))
      Math.min(Math.abs(x), Math.abs(y)) + (if (Math.abs(x) > Math.abs(y)) diff else diff / 2)
    }

    farthestDistance = Math.max(distance, farthestDistance)
  }

  var farthestDistance = 0
  val position = in.foldLeft(Hex(0, 0))(_ move _)

  println(s"part 1 - ${position.distance}\npart 2 - $farthestDistance")
}
