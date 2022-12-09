package com.taylorperkins.aoc.day9
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils
import com.taylorperkins.aoc.day9.Part2.Rope.concat

object Part2 extends App
  with Utils
{

  type Coord = Tuple2[Int, Int]

  implicit class CoordUtils(thisCoord: Coord)
  {
    def is(thatCoord: Coord) = thisCoord == thatCoord

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
    def apply(size: Int) = new Rope(List.fill(size)((0, 0)))
    def apply(knots: List[Coord]) = new Rope(knots)

    def simulate(movements: List[String])(size: Int) = {
      def inner(movements: List[String], ropes: List[Rope]): List[Rope] =
        if (movements.isEmpty) ropes
        else inner(movements.tail, ropes :+ ropes.last.move(movements.head))

      inner(movements, List(Rope(size)))
    }

    def concat(left: Rope, right: Rope): Rope =
      if (right.knots.isEmpty) left
      else concat(left.add(right.head), right.tail)
  }

  class Rope(val knots: List[Coord])
  {

    def head: Coord = knots.head
    def last: Coord = knots.last
    def tail: Rope = Rope(knots.tail)

    def add(knot: Coord): Rope = Rope(knots :+ knot)

    def move(direction: String): Rope = {
      def inner(from: Rope, acc: Rope): Rope =
        if (from.knots.isEmpty) acc
        // here, we shouldn't have to move the remaining knots if the current
        // knot didn't move. Just concat and move on
        else if (!knotShouldMove(acc.last, from.head)) concat(acc, from)
        else {
          val directions = from.head.directionsTowards(acc.last)
          val knot = from.head.move(directions)
          inner(from.tail, acc.add(knot))
        }

      inner(tail, Rope(size = 0).add(head.move(direction)))
    }

    def knotShouldMove(h: Coord, t: Coord) = (h._1-t._1).abs >= 2 || (h._2-t._2).abs >= 2
  }

  using(resource("src/main/resources/day9.txt")) { input => {

    // convert to a stream of single commands
    val movements = input.getLines.toList.map({ line =>
      line.split(" ") match {
        case Array(a, b) => Array.fill(b.toInt)(a)
      }
    }).flatten

    val steps = Rope.simulate(movements)(size = 10)
    val uniqueTailLocations = steps
      .map(_.last)
      .toSet
      .size

    println(uniqueTailLocations)
  }}
}
