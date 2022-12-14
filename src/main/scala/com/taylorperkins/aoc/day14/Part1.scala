package com.taylorperkins.aoc.day14
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.annotation.tailrec

object Part1 extends App
  with Utils
{
  type Coord = (Int, Int)

  // key -> x coord
  // value -> all y coords that hold an obstacle for that row
  def parseInput(input: Array[Coord]): Map[Int, Set[Int]] = {
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

  def simulate(grid: Map[Int, Set[Int]]): Int = {
    val start = (0, 500)
    // when the sand goes below the lowest row that has settled obstacles, it's done
    val lowestRow = grid.keys.max

    val directions = List(
      (coord: Coord) => (coord._1+1, coord._2),
      (coord: Coord) => (coord._1+1, coord._2-1),
      (coord: Coord) => (coord._1+1, coord._2+1)
    )

    @tailrec
    def simulate0(current: Coord, acc: Int, directionsRemaining: List[Coord => Coord])(implicit grid: Map[Int, Set[Int]]): Int = {
      if (current._1 > lowestRow) acc
        // settled, nothing else to do
      else if (directionsRemaining.isEmpty) simulate0(start, acc+1, directions)(grid + (current._1 -> (grid.getOrElse(current._1, Set.empty[Int])+current._2)))
      else {
        val nextStep = directionsRemaining.head(current)
        // try another direction
        if (gridContains(nextStep)) simulate0(current, acc, directionsRemaining.tail)
        else simulate0(nextStep, acc, directions)
      }
    }

    simulate0(start, 0, directions)(grid)
  }


  def simulatePt2(grid: Map[Int, Set[Int]]): Int = {
    val start = (0, 500)
    // sand can build on this row indefinitely
    val lowestRow = grid.keys.max + 2

    val directions = List(
      (coord: Coord) => (coord._1 + 1, coord._2),
      (coord: Coord) => (coord._1 + 1, coord._2 - 1),
      (coord: Coord) => (coord._1 + 1, coord._2 + 1)
    )

    @tailrec
    def simulate0(current: Coord, acc: Int, directionsRemaining: List[Coord => Coord])(implicit grid: Map[Int, Set[Int]]): Int = {
      if (directionsRemaining.isEmpty && current == start) acc + 1
      // settled, nothing else to do
      else if (directionsRemaining.isEmpty) simulate0(start, acc + 1, directions)(grid + (current._1 -> (grid.getOrElse(current._1, Set.empty[Int]) + current._2)))
      else {
        val nextStep = directionsRemaining.head(current)
        // try another direction
        if (gridContains(nextStep) || nextStep._1 == lowestRow) simulate0(current, acc, directionsRemaining.tail)
        else simulate0(nextStep, acc, directions)
      }
    }

    simulate0(start, 0, directions)(grid)
  }


  using(resource("src/main/resources/day14.txt")) { input => {

    val coords = input.getLines
      .map(line => {
        val coords: Array[Coord] = line
          .split(" -> ")
          .map(line => {
            val Array(a, b) = line.split(",").map(_.toInt)
            (b, a)
          })

        // here, we want to gather all coords that make up the lines in the row
        coords.foldLeft(Array.empty[Coord])
          ((a: Array[Coord], b: Coord) => {
            if (a.isEmpty) Array((b._1, b._2))
            else {

              // probably a better way to do this
              val coords = {
                if (a.last._1 < b._1) for {i <- a.last._1 to b._1} yield (i, a.last._2)
                else if (a.last._1 > b._1) (for {i <- b._1 to a.last._1} yield (i, a.last._2)).reverse
                else if (a.last._2 < b._2) for {i <- a.last._2 to b._2} yield (a.last._1, i)
                else (for {i <- b._2 to a.last._2} yield (a.last._1, i)).reverse
              }

              a ++ coords.tail
            }
          })
      })
      .flatten
      .toArray

    val mapSet = parseInput(coords)

//    val pt1 = simulate(mapSet)
    val pt2 = simulatePt2(mapSet)

    println(pt2)
  }}
}
