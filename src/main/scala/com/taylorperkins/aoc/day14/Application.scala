package com.taylorperkins.aoc.day14
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils
import com.taylorperkins.aoc.day14.Application.Coord

import scala.annotation.tailrec

object Application extends App
  with Utils
{
  type Coord = (Int, Int)

  def buildGrid(lines: String): Map[Int, Set[Int]] =
    toMapSet(
      lines
        .split("\n")
        .map(toCoords)
        .flatMap(groupCoords)
        .flatMap(toLine)
    )

  def toCoords(line: String): Array[Coord] =
    line
      .split(" -> ")
      .map(line => {
        val Array(a, b) = line.split(",").map(_.toInt)
        (b, a)
      })

  // groups lines of coords into individual lines, in order
  // It helps to sort them in this way up from so we can more easily
  // create the "line" down the road
  def groupCoords(coords: Array[Coord]): List[(Coord, Coord)] = {
    @tailrec
    def groupCoords0(coords: Array[Coord], acc: List[(Coord, Coord)]): List[(Coord, Coord)] = {
      val next = coords.take(2)
      if (next.length != 2) acc
      else {
        val Array(left: Coord, right: Coord) = next
        val x = List(left._1, right._1)
        val y = List(left._2, right._2)
        groupCoords0(coords.tail, acc :+ ((x.min, y.min), (x.max, y.max)))
      }
    }

    groupCoords0(coords, List.empty[(Coord, Coord)])
  }

  // get all coords associated with start and end coords (line)
  def toLine(pair: (Coord, Coord)): IndexedSeq[Coord] =
    for {
      x <- pair._1._1 to pair._2._1
      y <- pair._1._2 to pair._2._2
    } yield (x, y)

  // key -> x coord
  // value -> all y coords that hold an obstacle for that row
  def toMapSet(input: Array[Coord]): Map[Int, Set[Int]] = {
    @tailrec
    def parseInput0(input: Array[Coord], acc: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
      if (input.isEmpty) acc
      else {
        val (x, y) = input.head
        val newSet = acc.getOrElse(x, Set.empty[Int]) + y
        parseInput0(input.tail, acc + (x -> newSet))
      }
    }

    parseInput0(input, Map.empty[Int, Set[Int]])
  }

  def gridContains(coord: Coord)(implicit grid: Map[Int, Set[Int]]) =
    (grid contains coord._1) && (grid(coord._1) contains coord._2)

  def simulate(grid: Map[Int, Set[Int]])(part: String): Int = {
    val start = (0, 500)
    // when the sand goes below the lowest row that has settled obstacles, it's done
    val lowestRow = grid.keys.max

    val directions = List(
      (coord: Coord) => (coord._1+1, coord._2),
      (coord: Coord) => (coord._1+1, coord._2-1),
      (coord: Coord) => (coord._1+1, coord._2+1)
    )

    @tailrec
    def simulatePt1(current: Coord, acc: Int, directionsRemaining: List[Coord => Coord])(implicit grid: Map[Int, Set[Int]]): Int = {
      if (current._1 > lowestRow) acc
        // settled, nothing else to do
      else if (directionsRemaining.isEmpty) simulatePt1(start, acc+1, directions)(grid + (current._1 -> (grid.getOrElse(current._1, Set.empty[Int])+current._2)))
      else {
        val nextStep = directionsRemaining.head(current)
        // try another direction
        if (gridContains(nextStep)) simulatePt1(current, acc, directionsRemaining.tail)
        else simulatePt1(nextStep, acc, directions)
      }
    }

    @tailrec
    def simulatePt2(current: Coord, acc: Int, directionsRemaining: List[Coord => Coord])(implicit grid: Map[Int, Set[Int]]): Int = {
      if (directionsRemaining.isEmpty && current == start) acc + 1
      // settled, nothing else to do
      else if (directionsRemaining.isEmpty) simulatePt2(start, acc + 1, directions)(grid + (current._1 -> (grid.getOrElse(current._1, Set.empty[Int]) + current._2)))
      else {
        val nextStep = directionsRemaining.head(current)
        // try another direction
        if (gridContains(nextStep) || nextStep._1 == (lowestRow+2)) simulatePt2(current, acc, directionsRemaining.tail)
        else simulatePt2(nextStep, acc, directions)
      }
    }

    if (part == "pt1") simulatePt1(start, 0, directions)(grid)
    else simulatePt2(start, 0, directions)(grid)

  }


  using(resource("src/main/resources/day14.txt")) { input => {

    val lines = input.toAOC

    val grid = time { buildGrid(lines) }

    val pt1 = time { simulate(grid)("pt1") }
    println(pt1)

    val pt2 = time { simulate(grid)("pt2") }
    println(pt2)

  }}

}
