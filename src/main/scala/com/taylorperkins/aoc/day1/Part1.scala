package com.taylorperkins.aoc.day1
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part1 extends App
  with Utils
{
  using(resource("src/main/resources/day1.txt")) { input => {
    println(input.getLines().toList)
  }}
}
