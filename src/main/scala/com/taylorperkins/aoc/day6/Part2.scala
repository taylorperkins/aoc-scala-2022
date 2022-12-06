package com.taylorperkins.aoc.day6
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.annotation.tailrec

object Part2 extends App
  with Utils
{

  @tailrec
  def findPacketMarker(line: String, acc: Int = 0): Int = {
    if (line.take(14).toSet.size == 14) acc+14
    else findPacketMarker(line.drop(1), acc+1)
  }


  using(resource("src/main/resources/day6.txt")) { input => {

    input.getLines
      .map(line => findPacketMarker(line, 0))
      .foreach(println)

  }}
}
