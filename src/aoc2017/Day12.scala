package aoc2017

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day12 extends App{
  val line = """(\d+) <-> (.*)""".r
  val connections: Map[Int, Set[Int]] = (scala.io.Source.fromFile("src/aoc2017/day12.input.txt").getLines map {
    case line(id, connectedPrograms) => id.trim.toInt -> connectedPrograms.split(",").map(_.trim.toInt).toSet
  } toMap) withDefaultValue Set[Int]()

  @tailrec
  def findConnections(programsToProcess: Queue[Int], group: Set[Int]): Set[Int] = {
    if (programsToProcess.isEmpty)
      group
    else {
      val (programId: Int, remaining: Queue[Int]) = programsToProcess.dequeue
      val newConnections = connections(programId).diff(group).filterNot(remaining.contains)

      findConnections(remaining.enqueue(newConnections), group + programId)
    }
  }

  @tailrec
  def computeTotalNumberOfGroups(connections: Map[Int, Set[Int]], totalGroups: Int): Int = {
    if (connections.isEmpty)
      totalGroups
    else {
      val group = findConnections(Queue(connections.head._1), Set())
      computeTotalNumberOfGroups(connections.filterKeys(!group.contains(_)), totalGroups + 1)
    }
  }

  val groupWithZero = findConnections(Queue(0), Set())
  val totalNumberOfGroups = computeTotalNumberOfGroups(connections, 0)

  println(s"part 1 - ${groupWithZero.size}\npart 2 - $totalNumberOfGroups")
}
