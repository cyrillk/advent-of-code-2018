package adventofcode

import scala.collection.mutable
import scala.io.Source

object Day3 extends App {

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)

  def parse(str: String): Claim = {
    str.split(Array('#', '@', ':', ',', 'x')).map(_.trim).filter(_.nonEmpty).map(_.toInt).toList match {
      case id :: l :: t :: w :: h :: Nil => Claim(id, l, t, w, h)
      case _ => throw new UnsupportedOperationException("Unsupported")
    }
  }

  case class Pos(x: Int, y: Int)

  def overlaps(lines: Iterable[String]): Set[(Pos, Set[Int])] = {

    lines.map(parse).foldLeft(mutable.Map.empty[Pos, Seq[Int]].withDefaultValue(Nil)) {
      case (m, Claim(id, l, t, w, h)) =>
        for {
          x <- l until l + w
          y <- t until t + h
        } yield {
          m(Pos(x, y)) = m(Pos(x, y)) :+ id
        }
        m
    }.mapValues(_.toSet).toSet
  }

  def partOne(lines: Iterable[String]): Int = {
    overlaps(lines).count(_._2.size > 1)
  }

  def partTwo(lines: Iterable[String]): Int = {

    val points = overlaps(lines).map(_._2)

    (points.flatten -- points.filter(_.size > 1).flatten).headOption match {
      case Some(id) => id
      case _ => throw new UnsupportedOperationException("Damn")
    }
  }

  val input = Source.fromResource("day3input.txt").getLines().toStream

  val partOneInput = "#1 @ 1,3: 4x4" :: "#2 @ 3,1: 4x4" :: "#3 @ 5,5: 2x2" :: Nil
  val partTwoInput = "#1 @ 1,3: 4x4" :: "#2 @ 3,1: 4x4" :: "#3 @ 5,5: 2x2" :: Nil

  assert(partOne(partOneInput) == 4, s"Actual result is ${partOne(partOneInput)}")
  println(partOne(input))

  assert(partTwo(partTwoInput) == 3, s"Actual result is ${partTwo(partTwoInput)}")
  println(partTwo(input))
}