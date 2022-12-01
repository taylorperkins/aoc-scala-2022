package com.taylorperkins.aoc.day1
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils
import com.taylorperkins.aoc.day1.Part1.{resource, using}

object Part2 extends App
  with Utils
{
  using(resource("src/main/resources/day1.txt")) { input => {

    val result = input.toAOC
      .split("\n\n")
      .map(calories => {
        calories
          .split("\n")
          .map(_.toInt)
          .sum
      })
      .sorted(Ordering.Int.reverse)
      .take(3)
      .sum

    println(result)

  }}
}
