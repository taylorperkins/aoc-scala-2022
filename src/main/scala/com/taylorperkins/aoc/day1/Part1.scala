package com.taylorperkins.aoc.day1
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part1 extends App
  with Utils
{
  using(resource("src/main/resources/day1.txt")) { input => {

    val result = input.toAOC
      // each "elf's calories group" is denoted by a new line
      .split("\n\n")
      // for each elf's calories..
      .map(calories => {
        calories
          .split("\n")
          .map(_.toInt)
          .sum
      })
      .max

    println(result)
  }}
}
