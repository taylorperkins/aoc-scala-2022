package com.taylorperkins.aoc.day8
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part2 extends App
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

    def iter = for {
      row <- 0 to n-1
      col <- 0 to m-1
    } yield (row, col)

    def outOfBounds(coord: Coord): Boolean =
      coord._1 < 0
        || coord._1 >= n
        || coord._2 < 0
        || coord._2 >= m

    // return the coords matching the condition in the direction you specify
    // includes the first coord that does not match the condition
    def getWhile(condition: (Coord, Coord) => Boolean)(how: Coord => Coord)(from: Coord): List[Coord] = {
      def inner(originalCoord: Coord, nextCoord: Coord, acc: List[Coord]): List[Coord] =
        if (outOfBounds(nextCoord)) acc
        else if (condition(originalCoord, nextCoord)) inner(originalCoord, how(nextCoord), acc :+ nextCoord)
        else acc :+ nextCoord

      inner(originalCoord = from, nextCoord = how(from), acc = List.empty[Coord])
    }

    def iloc(coord: Coord) = input(coord._1)(coord._2)

    def scenicScores: List[Int] = {
      // comparing coord value is less than coord under observation
      val fn = getWhile((currentCoord, otherCoord) => iloc(otherCoord) < iloc(currentCoord))

      // do above in every direction
      val traverseFns = List(
        fn(_.left), fn(_.right),
        fn(_.up), fn(_.down)
      )

      iter
        .map(coord => {
          traverseFns
            // size represents the number of trees found
            .map(_(coord).size)
            .reduce(_ * _)
        })
        .toList
    }
  }

  using(resource("src/main/resources/day8.txt")) { input => {
    val trees = Matrix(input.getLines.map(_.map(_.asDigit).toList).toList)
    println(trees.scenicScores.max)
  }}
}
