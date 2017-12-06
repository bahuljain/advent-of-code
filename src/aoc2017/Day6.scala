package aoc2017

object Day6 extends App {
  val banks = scala.io.Source.fromFile("src/aoc2017/day6.input.txt").mkString.split("\\s+").map(_.toInt)
  val visitedState = scala.collection.mutable.Map[List[Int], Int](banks.toList -> 0)

  def findFirstRepeat(iteration: Int): (Int, Int) = {
    val (maxBankMoney, maxBankIndex) = banks.zipWithIndex.maxBy(_._1)
    banks(maxBankIndex) = 0

    (1 to maxBankMoney) foreach (i => banks((maxBankIndex + i) % banks.length) += 1)

    if (visitedState.contains(banks.toList))
      (iteration, iteration - visitedState(banks.toList))
    else {
      visitedState.put(banks.toList, iteration)
      findFirstRepeat(iteration + 1)
    }
  }

  val (part1, part2) = findFirstRepeat(1)
  println(s"part 1 - $part1\npart 2 - $part2")
}
