package aoc2017

object Day8 extends App {
  val instr = """(\w+) (\w+) (.*) if (\w+) (.*) (.*)""".r
  val registers = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
  var maxEver = 0

  def applyOp(op: String, a: Int, b: Int): Boolean = op match {
    case "!=" => a != b
    case ">=" => a >= b
    case "<=" => a <= b
    case "<" => a < b
    case ">" => a > b
    case "==" => a == b
    case _ => throw new Exception(s"Invalid operator - $op")
  }

  scala.io.Source.fromFile("src/aoc2017/day8.input.txt").getLines foreach {
    case instr(r1, action, by, r2, op, value) =>
      val (reg1, reg2) = (registers(r1), registers(r2))

      if (applyOp(op, reg2, value.toInt)) {
        val updatedValue = reg1 + ((if (action equals "inc") 1 else -1) * by.toInt)
        registers.update(r1, updatedValue)
        maxEver = if (updatedValue > maxEver) updatedValue else maxEver
      }
  }

  println(s"part 1 - ${registers.values.max}")
  println(s"part 2 - $maxEver")
}
