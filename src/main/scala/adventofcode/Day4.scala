package adventofcode

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.collection.mutable
import scala.io.Source

object Day4 extends App /*with AdventPartOne*/ {

  case class Sleep(start: Int, end: Option[Int])

  case class Guard(id: Int, sleep: Seq[Sleep] = Seq.empty[Sleep])

  def partOne(lines: Iterable[String]): Int = {

    val timestamp = """\[([\d\-]+\s\d\d\:\d\d)]\s.*""".r("timestamp")
    val beginsShift = """\[[\d\-\s\:]+]\sGuard\s\#(\d+)\sbegins shift""".r("id")
    val fallsAsleep = """\[[\d\-\s]+\:(\d\d)]\sfalls asleep""".r("min")
    val wakesUp = """\[[\d\-\s]+\:(\d\d)]\swakes up""".r("min")

    implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    val sorted = lines.toList.sortBy(s => LocalDateTime.parse(timestamp.findAllIn(s).group("timestamp"), formatter))

    val guards = sorted.foldLeft(Seq.empty[Guard]) {
      case (z, l) if beginsShift.findFirstIn(l).nonEmpty =>
        val id = beginsShift.findAllIn(l).group("id").toInt
        Guard(id) +: z
      case (h :: z, l) if fallsAsleep.findFirstIn(l).nonEmpty =>
        val min = fallsAsleep.findAllIn(l).group("min").toInt
        h.copy(sleep = Sleep(min, None) +: h.sleep) +: z
      case (h :: z, l) if wakesUp.findFirstIn(l).nonEmpty =>
        val min = wakesUp.findAllIn(l).group("min").toInt
        h.copy(sleep = h.sleep.head.copy(end = Some(min)) +: h.sleep.tail) +: z
      case _ =>
        throw new UnsupportedOperationException("ERROR")
    }

    val times = guards.groupBy(_.id).mapValues(_.flatMap(_.sleep).map {
      case Sleep(s, Some(e)) => (s, e)
      case _ => throw new UnsupportedOperationException("ERROR")
    })

    val guard = times.mapValues(_.map(p => p._2 - p._1).sum).toList.maxBy(_._2)._1

    val minute = times(guard).foldLeft(mutable.Map.empty[Int, Int].withDefaultValue(0)) { case (m, p) =>
      for (key <- p._1 until p._2) {
        m(key) = m(key) + 1
      }
      m
    }.maxBy(p => p._2)._1

    guard * minute
  }

  def partTwo(lines: Iterable[String]): Int = {

    val timestamp = """\[([\d\-]+\s\d\d\:\d\d)]\s.*""".r("timestamp")
    val beginsShift = """\[[\d\-\s\:]+]\sGuard\s\#(\d+)\sbegins shift""".r("id")
    val fallsAsleep = """\[[\d\-\s]+\:(\d\d)]\sfalls asleep""".r("min")
    val wakesUp = """\[[\d\-\s]+\:(\d\d)]\swakes up""".r("min")

    implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    val sorted = lines.toList.sortBy(s => LocalDateTime.parse(timestamp.findAllIn(s).group("timestamp"), formatter))

    val guards = sorted.foldLeft(Seq.empty[Guard]) {
      case (z, l) if beginsShift.findFirstIn(l).nonEmpty =>
        val id = beginsShift.findAllIn(l).group("id").toInt
        Guard(id) +: z
      case (h :: z, l) if fallsAsleep.findFirstIn(l).nonEmpty =>
        val min = fallsAsleep.findAllIn(l).group("min").toInt
        h.copy(sleep = Sleep(min, None) +: h.sleep) +: z
      case (h :: z, l) if wakesUp.findFirstIn(l).nonEmpty =>
        val min = wakesUp.findAllIn(l).group("min").toInt
        h.copy(sleep = h.sleep.head.copy(end = Some(min)) +: h.sleep.tail) +: z
      case _ =>
        throw new UnsupportedOperationException("ERROR")
    }

    val times = guards.groupBy(_.id).mapValues(_.flatMap(_.sleep).map {
      case Sleep(s, Some(e)) => (s, e)
      case _ => throw new UnsupportedOperationException("ERROR")
    })

    val agg = (0 until 60).map(minute =>
      (minute, times.mapValues(q => q.count(c => c._1 <= minute && minute < c._2)))).toMap

    val result = agg.mapValues(_.maxBy(_._2)).maxBy(_._2._2)

    result._1 * result._2._1
  }

  val input = Source.fromResource("day4input.txt").getLines().toStream

  val sampleInput =
    """
      |[1518-11-01 00:00] Guard #10 begins shift
      |[1518-11-01 00:05] falls asleep
      |[1518-11-01 00:25] wakes up
      |[1518-11-01 00:30] falls asleep
      |[1518-11-01 00:55] wakes up
      |[1518-11-01 23:58] Guard #99 begins shift
      |[1518-11-02 00:40] falls asleep
      |[1518-11-02 00:50] wakes up
      |[1518-11-03 00:05] Guard #10 begins shift
      |[1518-11-03 00:24] falls asleep
      |[1518-11-03 00:29] wakes up
      |[1518-11-04 00:02] Guard #99 begins shift
      |[1518-11-04 00:36] falls asleep
      |[1518-11-04 00:46] wakes up
      |[1518-11-05 00:03] Guard #99 begins shift
      |[1518-11-05 00:45] falls asleep
      |[1518-11-05 00:55] wakes up
    """.stripMargin.split("""\n""").map(_.trim).filter(_.nonEmpty)

  assert(partOne(sampleInput) == 240, s"Actual result is ${partOne(sampleInput)}")
  println(partOne(input))

  assert(partTwo(sampleInput) == 4455, s"Actual result is ${partTwo(sampleInput)}")
  println(partTwo(input))
}
