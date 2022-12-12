package com.taylorperkins.aoc.day12
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

object Part2 extends App
  with Utils
{
  val letters = ('a' to 'z').toList

  type Coord = (Int, Int)
  type Grid = Array[Array[Char]]

  implicit class GridUtils(grid: Grid) {
    // our graph is a matrix, so we can take advantage of some
    // matrix-type attributes
    val nRows: Int = grid.length
    val nCols: Int = grid(0).length

    def contains(coord: Coord): Boolean =
      coord._1 >= 0 && coord._1 < nRows
        && coord._2 >= 0 && coord._2 < nCols

    def apply(coord: Coord): Char = grid(coord._1)(coord._2)

    def neighbors(coord: Coord): List[(Int, Int)] = List(
      (coord._1 - 1, coord._2),
      (coord._1 + 1, coord._2),
      (coord._1, coord._2 + 1),
      (coord._1, coord._2 - 1)
    )
      .filter(grid.contains(_))

  }

  case class Vertex(id: Coord)(implicit grid: Grid)
  {
    val value: Char = grid(id._1)(id._2)
    def neighbors: List[Vertex] = grid.neighbors(id)
      .map(Vertex.apply)
      .filter(vertex => {
        if (vertex.value == 'E') value == 'z'
        else letters.indexOf(vertex.value) <= (letters.indexOf(value) + 1)
      })

    override def equals(obj: Any): Boolean =
      obj match
        case vertex: Vertex => vertex.id == id && vertex.value == value
        case _              => false
  }

  def dijkstra(start: Vertex)(criteria: Vertex => Boolean): Option[Int] = {

    @tailrec
    def updateConsidering(vertices: List[(Vertex, Int)], acc: Map[Vertex, Int]): Map[Vertex, Int] =
      if (vertices.isEmpty) acc
      else if (acc contains vertices.head._1)
        updateConsidering(vertices.tail, acc + (vertices.head._1 -> List(vertices.head._2, acc(vertices.head._1)).min))
      else updateConsidering(vertices.tail, acc + vertices.head)

    @tailrec
    def dijkstra0(currentNode: (Vertex, Int), considering: Map[Vertex, Int], visited: Set[Vertex]): Option[Int] = {
      if (criteria(currentNode._1)) Some(currentNode._2)
      else
        val neighbors = currentNode._1.neighbors
          .filterNot(v => visited.contains(v))
          .map((_, currentNode._2+1))

        val c = updateConsidering(neighbors, considering)

        val result = c
          .keysIterator
          .map(v => (v, c(v)))
          .find(_._2 == c.values.min)

        result match {
          case Some((v, int)) => dijkstra0((v, int), c - v, visited + currentNode._1)
          case None => None
        }
    }

    dijkstra0((start, 0), Map(start -> 0), Set(start))
  }

  using(resource("src/main/resources/day12.txt")) { input => {

    implicit val grid: Grid = input.getLines.map(_.replace("S", "a").toCharArray).toArray
    val vertices = for {
      (row, rowInd) <- grid.zipWithIndex
      (_, colInd)   <- row.zipWithIndex
    } yield Vertex(id = (rowInd, colInd))

    // just going at all options
    val result = vertices
      .filter(_.value == 'a')
      .flatMap(vertex => dijkstra(vertex)(_.value == 'E'))
      .min

    println(result)
  }}
}
