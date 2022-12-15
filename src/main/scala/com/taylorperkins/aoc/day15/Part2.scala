package com.taylorperkins.aoc.day15
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part2 extends App
  with Utils
{

  case class Range(start: Int, stop: Int)
  {
    def contains(i: Int): Boolean = start <= i && i <= stop
    def contains(r: Range): Boolean = contains(r.start) && contains(r.stop)

    def merge(r: Range): Option[Range] =
      if (contains(r)) Some(this)
      else if (contains(r.start)) Some(this.copy(stop = r.stop))
      else if (contains(r.stop)) Some(this.copy(start = r.start))
      else None

    def iter: IndexedSeq[Int] = for {i <- start to stop} yield i

    // +1 b/c inclusive
    def size: Int = stop - start + 1
  }



  case class Coord(col: Int, row: Int)
  {
    def manhattan(that: Coord): Int = (col - that.col).abs + (row - that.row).abs
  }

  case class Sensor(loc: Coord, closestBeacon: Coord)
  {
    def visibility: Int = loc.manhattan(closestBeacon)

    def vizAt(row: Int): Option[Range] = {
      val colDist = visibility - (loc.row - row).abs
      if (colDist < 0) None
      else Some(Range(loc.col-colDist, loc.col+colDist))
    }
  }

  def mergeRange(ranges: List[Range], range: Range): List[Range] = {
    if (ranges.isEmpty) List(range)
    else
      val next = ranges.last.merge(range) match
        case Some(value) => List(value)
        case None => List(ranges.last, range)

      ranges.dropRight(1) ++ next
  }

  using(resource("src/main/resources/day15.txt")) { input => {

    val inputRE = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

    val sensors = time {
      input.getLines
        .map(line => {
          line match
            case inputRE(sx, sy, bx, by) => Sensor(loc = Coord(sx.toInt, sy.toInt), closestBeacon = Coord(bx.toInt, by.toInt))
        })
        .toList
    }

    time {
      (0 to 4000000)
        .flatMap(row => {
          val ranges = sensors.flatMap(_.vizAt(row))
            .sortWith(_.start < _.start)
            .foldLeft(List.empty[Range])(mergeRange)
            .dropWhile(_.stop < 0)

          ranges match {
            case List(r1, r2) =>
              if (r1.stop+1 != r2.start) Some(Coord(row = row, col = r1.stop+1))
              else None
            case _ => None
          }
        })
        .map(coord => coord.col*BigInt(4000000)+coord.row)
        .foreach(println)
    }
  }}

}
