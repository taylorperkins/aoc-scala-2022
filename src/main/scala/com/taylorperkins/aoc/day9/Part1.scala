package com.taylorperkins.aoc.day9
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part1 extends App
  with Utils
{

  type Coord = Tuple2[Int, Int]

  implicit class CoordUtils(thisCoord: Coord)
  {
    def move(direction: String): Coord = direction match {
      case "L" => (thisCoord._1-1, thisCoord._2)
      case "R" => (thisCoord._1+1, thisCoord._2)
      case "U" => (thisCoord._1, thisCoord._2+1)
      case "D" => (thisCoord._1, thisCoord._2-1)
    }

    def move(directions: List[String]): Coord =
      if (directions.isEmpty) thisCoord
      else move(directions.head).move(directions.tail)

    def directionsTowards(thatCoord: Coord) =
      List(
        (thatCoord._1 > thisCoord._1, "R"),
        (thatCoord._1 < thisCoord._1, "L"),
        (thatCoord._2 > thisCoord._2, "U"),
        (thatCoord._2 < thisCoord._2, "D"),
      ).filter(_._1).map(_._2)
  }

  object Rope
  {
    def apply() = new Rope((0, 0), (0, 0))
    def apply(head: Coord, tail: Coord) = new Rope(head, tail)

    def simulate(movements: List[String]) = {
      def inner(movements: List[String], ropes: List[Rope]): List[Rope] =
        if (movements.isEmpty) ropes
        else inner(movements.tail, ropes :+ ropes.last.move(movements.head))

      inner(movements, List(Rope()))
    }
  }

  class Rope(val head: Coord, val tail: Coord)
  {
    def move(direction: String) = {
      val r = Rope(head.move(direction), tail)
      if (!r.tailShouldMove) r
      else {
        val directions = r.tail.directionsTowards(r.head)
        Rope(r.head, r.tail.move(directions))
      }
    }

    def tailShouldMove = (head._1-tail._1).abs >= 2 || (head._2-tail._2).abs >= 2

  }

  using(resource("src/main/resources/day9.txt")) { input => {

    // convert to a stream of single commands
    val movements = input.getLines.toList.map({ line =>
      line.split(" ") match {
        case Array(a, b) => Array.fill(b.toInt)(a)
      }
    }).flatten

    val steps = Rope.simulate(movements)
    val uniqueTailLocations = steps
      .map(_.tail)
      .toSet
      .size

    println(uniqueTailLocations)

  }}
}
