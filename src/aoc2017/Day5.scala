package aoc2017

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {
  val in = Source.fromFile("src/aoc2017/day5.input.txt").getLines.map(_.toInt).toArray

  def solution(seq: Array[Int], fn: (Int) => Int): Int = {
    @tailrec
    def getExitStep(pos: Int, steps: Int): Int = {
      if (pos >= seq.length || pos < 0)
        steps
      else {
        val jump = seq(pos)
        seq(pos) = jump + fn(if (jump >= 3) -1 else 1)
        getExitStep(pos +  jump, steps + 1)
      }

    }
    getExitStep(0, 0)
  }


  println("part 1 - " + solution(in.clone(), Math.abs))
  println("part 2 - " + solution(in.clone(), identity))
}
