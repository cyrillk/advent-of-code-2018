package adventofcode

import scala.io.Source

object Day2 extends App {

  def partOne(lines: Iterable[String]): Int = {
    val appears = lines.map(s => s.toCharArray.groupBy(c => c).values.map(_.length).toSet)
    appears.count(_ (2)) * appears.count(_ (3))
  }

  def partTwo(lines: Iterable[String]): String = {

    def cut(index: Int, s: String) = s.substring(0, index) + s.substring(index + 1, s.length)

    def inner(index: Int): String = {
      if (lines.headOption.forall(s => index >= s.length)) return ""

      val maybeResult = lines
        .map(s => cut(index, s))
        .groupBy(s => s)
        .mapValues(_.size)
        .find(_._2 > 1)
        .map(_._1)

      maybeResult match {
        case None if lines.headOption.forall(s => index + 1 < s.length) => inner(index + 1)
        case Some(s) => s
        case _ => "error"
      }
    }

    inner(0)
  }

  val input = Source.fromResource("day2input.txt").getLines().toStream

  val partOneInput = "abcdef" :: "bababc" :: "abbcde" :: "abcccd" :: "aabcdd" :: "abcdee" :: "ababab" :: Nil
  val partTwoInput = "abcde" :: "fghij" :: "klmno" :: "pqrst" :: "fguij" :: "axcye" :: "wvxyz" :: Nil

  assert(partOne(partOneInput) == 12, s"Actual result is ${partOne(partTwoInput)}")
  println(partOne(input))

  assert(partTwo(partTwoInput) == "fgij", s"Actual result is ${partTwo(partTwoInput)}")
  println(partTwo(input))
}