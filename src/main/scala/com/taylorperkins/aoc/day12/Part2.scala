package com.taylorperkins.aoc.day12
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part2 extends App
  with Utils
{
  using(resource("src/main/resources/day12-test.txt")) { input => {
    println(input.getLines().toList)
  }}
}
