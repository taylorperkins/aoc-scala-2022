package com.taylorperkins.aoc.day3
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.collection.immutable.HashSet


object Part2 extends App
  with Utils
{
  val ids = ('a' to 'z') ++ ('A' to 'Z')

  using(resource("src/main/resources/day3.txt")) { input => {
    val result = input.getLines
      .grouped(3)
      .map(_.toList match {
        case List(a, b, c) => a.toSet.intersect(b.toSet).intersect(c.toSet).head
      })
      .map(ids.indexOf(_) + 1)
      .sum

    println(result)
  }}
}
