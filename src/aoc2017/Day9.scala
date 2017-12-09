package aoc2017

object Day9 extends App {
  val in = scala.io.Source.fromFile("src/aoc2017/day9.input.txt").mkString

  def solution(s: List[Char], score: Int, garbage: Int, depth: Int, ignore: Boolean): (Int, Int) = s match {
    case Nil => (score, garbage)
    case '!' :: _ :: tail => solution(tail, score, garbage, depth, ignore)
    case '>' ::  tail if ignore => solution(tail, score, garbage, depth, ignore = false)
    case _ :: tail if ignore => solution(tail, score, garbage + 1, depth, ignore)
    case '<' :: tail => solution(tail, score, garbage, depth, ignore = true)
    case '{' :: tail => solution(tail, score, garbage, depth + 1, ignore)
    case '}' :: tail => solution(tail, score + depth, garbage, depth - 1, ignore)
    case _ :: tail => solution(tail, score, garbage, depth, ignore)
  }

  val (score, garbageCount) = solution(in.toList, 0, 0, 0, ignore = false)
  println(s"part 1 - $score\npart 2 - $garbageCount")

  assert(score == 9662)
  assert(garbageCount == 4903)
}
