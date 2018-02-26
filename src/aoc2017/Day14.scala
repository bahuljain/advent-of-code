package aoc2017

import scala.collection.immutable.Queue

object Day14 extends App {
  type Pos = (Int, Int)

  val in = "jzgqcdpd"

  def hexToBinary(hex: Char): String = {
    val int = Integer.parseInt(hex.toString, 16)
    String.format("%4s", Integer.toBinaryString(int)).replace(' ', '0')
  }

  val grid = List.tabulate(128)(i => s"$in-$i")
    .map(Day10.knotHash)
    .map(_.map(hexToBinary).mkString)

  val used = grid
    .map(_.count(_ == '1'))
    .sum

  val usedIndices = grid.zipWithIndex flatMap {
    case (row, rowIdx) => row.zipWithIndex.filter(_._1 == '1').map(col => (col._2, rowIdx))
  } toSet

  def countRegions(usedIndices: Set[Pos]): Int = {
    def count(queue: Queue[Pos], usedIndices: Set[Pos], noOfRegions: Int): Int = {
      if (queue.isEmpty) {
        if (usedIndices.isEmpty)
          noOfRegions
        else
          count(queue.enqueue(usedIndices.head), usedIndices.tail, noOfRegions + 1)
      } else {
        val ((x, y), rest) = queue.dequeue
        val sameRegionPoints = List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).filter(usedIndices.contains)

        count(rest enqueue sameRegionPoints, usedIndices -- sameRegionPoints, noOfRegions)
      }
    }

    if (usedIndices.isEmpty)
      0
    else
      count(Queue(usedIndices.head), usedIndices.tail, 1)
  }

  println(s"part 1 - $used\npart 2 - ${countRegions(usedIndices)}")
}
