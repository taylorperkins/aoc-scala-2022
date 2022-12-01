package com.taylorperkins.aoc.day1
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part2 extends App
  with Utils
{
  using(resource("src/main/resources/day1.txt")) { input => {

    val result = input.toAOC
      // each "elf's calories group" is denoted by a new line
      .split("\n\n")
      // for each elf's calories..
      .map(_.split("\n").map(_.toInt).sum)
      .sorted(Ordering.Int.reverse)
      .take(3)
      .sum

    println(result)

  }}
}
