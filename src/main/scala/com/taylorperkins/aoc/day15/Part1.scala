package com.taylorperkins.aoc.day15
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part1 extends App
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

    val sensors = input.getLines
      .map(line => {
        line match
          case inputRE(sx, sy, bx, by) => Sensor(loc = Coord(sx.toInt, sy.toInt), closestBeacon = Coord(bx.toInt, by.toInt))
      })
      .toList

    val row = 2000000

    val ranges = sensors
      .map(_.vizAt(row))
      .filterNot(_.isEmpty)
      .map(_.get)
      .sortWith(_.start < _.start)
      .foldLeft(List.empty[Range])(mergeRange)

    val visibility = ranges.map(_.size).sum

    val existingBeacons = sensors
      .map(_.closestBeacon)
      .filter(_.row == row)
      .map(_.col)
      .toSet
      .size

    println(ranges)
    println(visibility - existingBeacons)

    //    val ranges = mergeRanges(sensors.map(_.vizAt(row)))
    //
    //    val visibility = sensors
    //      .flatMap(_.vizAt(row))
    //      .toSet
    //      .size
    //
    //
    //    println(visibility - existingBeacons)
  }}
}
