package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  def partOne(lines: Iterable[String]): Int = {
    lines.map(_.replaceFirst("""\+""", "").toInt).sum
  }

  def partTwo(lines: Iterable[String]): Int = {

    val numbers = lines.map(_.replaceFirst("""\+""", "").toInt).toArray

    @tailrec
    def partTwoRec(set: Set[Int], last: Int, index: Int): Int = {
      val next = if (index < numbers.length - 1) index + 1 else 0
      val num = last + numbers(index)

      if (set(num)) num else partTwoRec(set + num, num, next)
    }

    partTwoRec(Set.empty[Int], 0, 0)
  }

  val input = Source.fromResource("day1input.txt").getLines().toStream

  println(partOne(input))
  println(partTwo(input))
}
