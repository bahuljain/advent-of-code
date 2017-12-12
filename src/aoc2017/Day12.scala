package aoc2017

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day12 extends App {
  val line = """(\d+) <-> (.*)""".r
  val connections: Map[Int, Set[Int]] = (scala.io.Source.fromFile("src/aoc2017/day12.input.txt").getLines map {
    case line(id, connectedPrograms) => id.trim.toInt -> connectedPrograms.split(",").map(_.trim.toInt).toSet
  } toMap) withDefaultValue Set[Int]()

  @tailrec
  def findConnections(programsToProcess: Queue[Int], group: Set[Int] = Set()): Set[Int] =
    if (programsToProcess.isEmpty)
      group
    else {
      val (programId: Int, remaining: Queue[Int]) = programsToProcess.dequeue
      val newConnections = connections(programId).diff(group).filterNot(remaining.contains)

      findConnections(remaining.enqueue(newConnections), group + programId)
    }

  @tailrec
  def getGroupsCount(programs: Set[Int], groupCount: Int = 0): Int =
    if (programs.isEmpty)
      groupCount
    else
      getGroupsCount(programs.diff(findConnections(Queue(programs.head))), groupCount + 1)

  println(s"part 1 - ${findConnections(Queue(0)).size}\npart 2 - ${getGroupsCount(connections.keySet)}")
}
