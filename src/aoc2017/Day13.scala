package aoc2017

object Day13 extends App {

  case class FirewallLayer(depth: Int, range: Int) {
    def hasCaughtAtTime(time: Int): Boolean = time % (2 * range - 2) == 0

    def penalty(time: Int): Int = if (hasCaughtAtTime(time)) depth * range else 0
  }

  val line = """(\d+): (\d+)""".r

  val firewall = scala.io.Source.fromFile("src/aoc2017/day13.input.txt").getLines map {
    case line(depth, range) => depth.toInt -> FirewallLayer(depth.toInt, range.toInt)
  } toMap

  val maxDepth = firewall.keys.max

  val totalPenaltyIncurred: Int =
    (0 to maxDepth) map { depth =>
      firewall.get(depth).map(_.penalty(depth)).getOrElse(0)
    } sum

  val delayToAvoidGettingCaught: Int = {
    def hasPassedFirewall(delay: Int): Boolean =
      (0 to maxDepth) forall { depth =>
        firewall.get(depth).forall(!_.hasCaughtAtTime(depth + delay))
      }

    Stream.from(0).find(hasPassedFirewall).get
  }

  println(s"part 1 - $totalPenaltyIncurred\npart 2 - $delayToAvoidGettingCaught")
}
