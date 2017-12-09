package aoc2017

import scala.annotation.tailrec

object Day9 extends App {
  val in = scala.io.Source.fromFile("src/aoc2017/day9.input.txt").mkString

  @tailrec
  def solution(s: List[Char], score: Int, garbage: Int, depth: Int, inGarbage: Boolean): (Int, Int) = s match {
    case Nil => (score, garbage)
    case '!' :: _ :: tail => solution(tail, score, garbage, depth, inGarbage)
    case '>' ::  tail if inGarbage => solution(tail, score, garbage, depth, inGarbage = false)
    case _ :: tail if inGarbage => solution(tail, score, garbage + 1, depth, inGarbage)
    case '<' :: tail => solution(tail, score, garbage, depth, inGarbage = true)
    case '{' :: tail => solution(tail, score, garbage, depth + 1, inGarbage)
    case '}' :: tail => solution(tail, score + depth, garbage, depth - 1, inGarbage)
    case _ :: tail => solution(tail, score, garbage, depth, inGarbage)
  }

  val (score, garbageCount) = solution(in.toList, 0, 0, 0, inGarbage = false)
  println(s"part 1 - $score\npart 2 - $garbageCount")

  assert(score == 9662)
  assert(garbageCount == 4903)
}
