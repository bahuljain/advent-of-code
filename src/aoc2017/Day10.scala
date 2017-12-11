package aoc2017

object Day10 extends App {
  val in = "14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244"

  def hasher(lengths: Array[Int], rounds: Int): Array[Int] = {
    var (pos, skip) = (0, 0)
    val seq = Array.range(0, 256)

    (1 to rounds) foreach { _ =>
      lengths foreach { length =>
        Stream.continually(seq).flatten.slice(pos, pos + length).reverse.zipWithIndex foreach {
          case (value, idx) => seq((pos + idx) % 256) = value
        }

        pos = (pos + length + skip) % 256
        skip += 1
      }
    }
    seq
  }

  val part1 = hasher(in.split(",").map(_.toInt), 1).take(2).product

  val part2: String =
    hasher(in.toCharArray.map(_.toInt) ++ Array(17, 31, 73, 47, 23), 64)
      .grouped(16)
      .map(_.reduceLeft(_ ^ _))
      .map(i => "%02X".format(i))
      .mkString

  println(s"part 1 - $part1")
  println(s"part 2 - $part2")
}
