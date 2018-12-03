package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  def partOne(): Unit = {
    val result = Source.fromResource("day1input.txt").getLines().toStream.foldLeft(0) { case (z, item) =>
      if (item.startsWith("+")) z + item.drop(1).toInt else z - item.drop(1).toInt
    }

    println(result)
  }

  def partTwo(): Unit = {

    val lines = Source.fromResource("day1input.txt").getLines().toArray

    @tailrec
    def carryOn(set: Set[Int], z: Int, index: Int): Int = {
      val line = lines(index)
      val num = if (line.startsWith("+")) z + line.drop(1).toInt else z - line.drop(1).toInt
      val next = if (index < lines.length - 1) index + 1 else 0

      if (set(num)) num else carryOn(set + num, num, next)
    }

    val result = carryOn(Set.empty[Int], 0, 0)

    println(result)
  }

  partOne()
  partTwo()
}
