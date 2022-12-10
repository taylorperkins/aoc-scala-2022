package com.taylorperkins.aoc.day10
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part2 extends App
  with Utils
{

  type Pixel = String

  object CathodeRayTube {
    // outputs our final result as a list of "pixels"
    def apply(commands: List[String]): CathodeRayTube = {
      def inner(ctr: CathodeRayTube, commands: List[String]): CathodeRayTube =
        if (commands.isEmpty) ctr
        else inner(ctr(commands.head), commands.tail)

      inner(CathodeRayTube(), commands)
    }
  }

  case class CathodeRayTube(xRegister: Int = 1, pixels: List[Pixel] = List.empty[Pixel])
  {
    def apply(command: String): CathodeRayTube =
      val addxRE = "addx (.*)".r
      command match {
        case "noop"         => noop
        case addxRE(value)  => addX(value.toInt)
      }

    def noop: CathodeRayTube = draw

    def addX(value: Int): CathodeRayTube = draw.draw.copy(xRegister + value)

    def spritePositions = List(xRegister-1, xRegister, xRegister+1)

    def draw = CathodeRayTube(xRegister, pixels :+ nextPixel)

    def nextPixel =
      if (spritePositions.contains(loc(pixels.size)._2)) "#"
      else "."

    def loc(idx: Int): Tuple2[Int, Int] = (idx / 40, idx % 40)
  }

  using(resource("src/main/resources/day10.txt")) { input => {
    val commands = input.getLines.toList
    CathodeRayTube(commands)
      .pixels
        .grouped(40)
        .map(_.mkString)
        .foreach(println)
  }}
}
