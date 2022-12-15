package com.taylorperkins.aoc.day15
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part1 extends App
  with Utils
{

  case class Coord(col: Int, row: Int)
  {
    def manhattan(that: Coord): Int = (col - that.col).abs + (row - that.row).abs
  }

  case class Sensor(loc: Coord, closestBeacon: Coord)
  {
    def visibility: Int = loc.manhattan(closestBeacon)

    def vizAt(row: Int): IndexedSeq[Coord] = {
      val colDist = visibility - (loc.row - row).abs
      if (colDist < 0) IndexedSeq.empty[Coord]
      else for { col <- loc.col-colDist to loc.col+colDist} yield Coord(col = col, row = row)
    }
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

    val visibility = sensors
      .flatMap(_.vizAt(row))
      .toSet
      .size

    val existingBeacons = sensors
      .map(_.closestBeacon)
      .filter(_.row == row)
      .toSet
      .size

    println(visibility - existingBeacons)
  }}
}
