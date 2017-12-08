package aoc2017

import scala.collection.mutable
import scala.io.Source

object Day8 extends App {
  val instr = """(\w+) (\w+) (-)?(\d+) if (\w+) (.*) (-)?(\d+)""".r
  val registers = mutable.Map[String, Int]()
  var maxEver = 0

  def ops(op: String, a: Int, b: Int): Boolean = op match {
    case "!=" => a != b
    case ">=" => a >= b
    case "<=" => a <= b
    case "<" => a < b
    case ">" => a > b
    case "==" => a == b
    case _ => throw new Exception("Invalid operator")
  }

  Source.fromFile("src/aoc2017/day8.input.txt").getLines.toList foreach {
    case instr(r1, b, c, d, r2, op, g, h) =>
      val reg1 = registers.getOrElse(r1, 0)
      val reg2 = registers.getOrElse(r2, 0)
      val offset = (if (b equals "inc") 1 else -1) * Option(c).map(_ => -1).getOrElse(1) * d.toInt
      val value = (if (g == null) 1 else -1) * h.toInt

      if (ops(op, reg2, value)) {
        val updatedValue = reg1 + offset
        registers.update(r1, updatedValue)

        if (updatedValue > maxEver)
          maxEver = updatedValue
      }
  }

  println(s"part 1 - ${registers.values.max}")
  println(s"part 2 - $maxEver")
}
