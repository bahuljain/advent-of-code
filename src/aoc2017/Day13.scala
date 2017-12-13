package aoc2017

object Day13 extends App {

  case class Scanner(depth: Int, range: Int) {
    def hasPassed(delay: Int = 0): Boolean = (delay + depth) % (2 * range - 2) != 0

    def penalty: Int = if (hasPassed()) 0 else depth * range
  }

  val line = """(\d+): (\d+)""".r

  val firewall = scala.io.Source.fromFile("src/aoc2017/day13.input.txt").getLines map {
    case line(depth, range) => depth.toInt -> Scanner(depth.toInt, range.toInt)
  } toMap

  val maxDepth = firewall.keys.max

  val penalty = (0 to maxDepth).map(depth => firewall.get(depth).map(_.penalty).getOrElse(0)).sum

  def hasPassedAll(delay: Int) = 0 to maxDepth forall (depth => firewall.get(depth).forall(_.hasPassed(delay)))

  val delay = Stream.from(0).find(hasPassedAll).get

  println(s"part 1 - $penalty\npart 2 - $delay")
}
