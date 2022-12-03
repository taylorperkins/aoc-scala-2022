package com.taylorperkins.aoc.day3
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.collection.immutable.HashSet

object Part1 extends App
  with Utils
{
  val ids = ('a' to 'z') ++ ('A' to 'Z')

  using(resource("src/main/resources/day3.txt")) { input => {
    val result = input.getLines
      .map(str => str.grouped(str.length / 2).toList match {
        case List(a, b) => a.toSet.intersect(b.toSet).head
      })
      .map(ids.indexOf(_) + 1)
      .sum

    println(result)
  }}
}
