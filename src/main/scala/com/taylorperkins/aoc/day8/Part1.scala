package com.taylorperkins.aoc.day8
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part1 extends App
  with Utils
{
  type Coord = Tuple2[Int, Int]

  implicit class CoordUtil(coord: Coord)
  {
    def left = (coord._1, coord._2-1)
    def right = (coord._1, coord._2+1)
    def up = (coord._1-1, coord._2)
    def down = (coord._1+1, coord._2)
  }


  case class Matrix(input: List[List[Int]])
  {
    val n = input.size
    val m = input(0).size

    val edges = List(
      (for { num <- 0 until n } yield List((num, 0), (num, m-1))).flatten
      ++ (for { num <- 1 until m } yield List((0, num), (n-1, num))).flatten
    ).flatten

    def iter = for {
      row <- 0 to n-1
      col <- 0 to m-1
    } yield (row, col)

    def outOfBounds(coord: Coord): Boolean =
      coord._1 < 0
        || coord._1 >= n
        || coord._2 < 0
        || coord._2 >= m

    // return the coord matching the condition in the direction you specify
    def traverseUntil(condition: (Coord, Coord) => Boolean)(how: Coord => Coord)(from: Coord): Option[Coord] = {
      def inner(originalCoord: Coord, nextCoord: Coord): Option[Coord] =
        if (outOfBounds(nextCoord)) None
        else if (condition(originalCoord, nextCoord)) Some(nextCoord)
        else inner(originalCoord, how(nextCoord))

      inner(originalCoord = from, nextCoord = how(from))
    }

    def iloc(coord: Coord) = input(coord._1)(coord._2)
  }

  using(resource("src/main/resources/day8.txt")) { input => {

    val matrix = Matrix(input.getLines.map(_.map(_.asDigit).toList).toList)

    def isVisible(coord: Coord, traversals: List[(Coord) => Option[Coord]]): Int =
      if (traversals.isEmpty) 0
      else {
        traversals.head(coord) match {
          // met the condition (found a taller tree), so continue
          case Some(_) => isVisible(coord, traversals.tail)
          case None    => 1
        }
      }

    val traverseUntil = matrix.traverseUntil((currentCoord, otherCoord) => matrix.iloc(otherCoord) >= matrix.iloc(currentCoord))

    val traverseFns = List(
      traverseUntil(_.left),
      traverseUntil(_.right),
      traverseUntil(_.up),
      traverseUntil(_.down)
    )

    val result = matrix
      .iter
      .map(coord => {
        if (matrix.edges.contains(coord)) 1
        else isVisible(coord, traverseFns)
      })
      .sum

    println(result)

  }}
}
