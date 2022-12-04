package com.taylorperkins.aoc.day4
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part2 extends App
  with Utils
{

  using(resource("src/main/resources/day4.txt")) { input => {
    val result = input.getLines
      .map(line => {
        val Array(left, right) = line.split(",")
        val Array(a, b) = left.split("-").map(_.toInt)
        val Array(y, z) = right.split("-").map(_.toInt)

        if ((a to b).toSet.intersect((y to z).toSet).size > 0) 1
        else 0
      })
      .sum

    println(result)
  }}
}
