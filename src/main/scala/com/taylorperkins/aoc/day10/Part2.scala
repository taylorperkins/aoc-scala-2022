package com.taylorperkins.aoc.day10
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part2 extends App
  with Utils
{

  type Pixel = String


  case class CathodeRayTube(xRegister: Int = 1, pixels: List[Pixel] = List.empty[Pixel])
  {
    // run a function based on a command, creates a new CathodeRayTube
    def apply(command: String): CathodeRayTube =
      val addxRE = "addx (.*)".r
      command match {
        case "noop"         => noop
        case addxRE(value)  => addX(value.toInt)
      }

    // doesn't do much - just draws the next pixel
    def noop: CathodeRayTube = draw

    // draws twice, then updates the xRegister
    def addX(value: Int): CathodeRayTube = draw.draw.copy(xRegister + value)

    def spritePositions = List(xRegister-1, xRegister, xRegister+1)

    def draw = this.copy(pixels = pixels :+ nextPixel)

    def nextPixel =
      if (spritePositions.contains(loc(pixels.size)._2)) "#"
      else "."

    // this fn is the price I paid for deciding not to structure my pixels (screen) as a matrix.
    // worth it to me. Just determines the screen location based on an index, or what pixel you're on
    def loc(idx: Int): Tuple2[Int, Int] = (idx / CathodeRayTube.screenSize, idx % CathodeRayTube.screenSize)
  }

  object CathodeRayTube {

    val screenSize = 40

    // takes in all commands and passes them to a CathodeRayTube
    // to be processed, building from scratch.
    def apply(commands: List[String]): CathodeRayTube = {
      def inner(ctr: CathodeRayTube, commands: List[String]): CathodeRayTube =
        if (commands.isEmpty) ctr
        else inner(ctr(commands.head), commands.tail)

      inner(CathodeRayTube(), commands)
    }
  }

  using(resource("src/main/resources/day10.txt")) { input => {
    val commands = input.getLines.toList

    CathodeRayTube(commands)
      .pixels
        .grouped(CathodeRayTube.screenSize)
        .map(_.mkString)
        .foreach(println)
  }}
}
