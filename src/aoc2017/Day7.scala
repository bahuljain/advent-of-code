package aoc2017

import scala.io.Source

/**
  * root - veboyvy
  * Fixed weight should be - 749
  */
object Day7 extends App {
  val line = """(\w+) \((\d+)\)(?: -> (.*))?""".r

  val in = Source.fromFile("src/aoc2017/day7.input.txt").getLines.toList map {
    case line(name, weight, children) => name ->
      (weight.toInt, Option(children).map(_.split(", ").toSet).getOrElse(Set()))
  } toMap

  val root: String = {
    def findRoot(name: String): String =
      in.find(e => e._2._2.contains(name)) match {
        case Some(e) => findRoot(e._1)
        case None => name
      }

    findRoot(in.head._1)
  }

  def fixWeights(sums: List[Int], children: Set[String]): Int = {
    val childrenWeights = children.toList.map(in).map(_._1)
    val uniqueSums = sums.groupBy(identity).mapValues(_.length)

    if (uniqueSums.size == 2) {
      val (sum1, count1) = uniqueSums.head
      val (sum2, count2) = uniqueSums.last

      val (oldWeight, adj) = if (count1 > count2)
        (childrenWeights(sums.indexOf(sum2)), sum1 - sum2)
      else
        (childrenWeights(sums.indexOf(sum1)), sum2 - sum1)

      println(s"Fixed weight should be - ${oldWeight + adj}")
      sums.sum + adj
    } else
      sums.sum
  }

  def towerSums(root: String): Int = {
    val (weight, children) = in(root)

    if (children.isEmpty)
      weight
    else {
      val sums = children.toList.map(towerSums)
      val totalWeightOnTower = fixWeights(sums, children)
      totalWeightOnTower + weight
    }
  }

  println(s"root - $root")
  towerSums(root)
}
