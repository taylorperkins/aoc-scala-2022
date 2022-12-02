package com.taylorperkins.aoc.day2
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils


object Part2 extends App
  with Utils
{
  object RPSEnum extends Enumeration {
    type Object = Value
    val Rock, Paper, Scissors = Value
  }

  import RPSEnum._

  case class RPSValue(label: Object, value: Int, win: Object, loss: Object) {
    def play(other: RPSValue): Int = other.label match
      case `label`   => 3 + value
      case `win`     => 6 + value
      case `loss`    => 0 + value
  }

  val rock = RPSValue(Rock, 1, Scissors, Paper)
  val paper = RPSValue(Paper, 2, Rock, Scissors)
  val scissors = RPSValue(Scissors, 3, Paper, Rock)

  // implicit type conversions!! Not something I use all the time, but definitely helpful
  // in keeping your general logic straight and to the point
  implicit def toRPS(s: String): RPSValue = s match {
    case "A" | "X" => rock
    case "B" | "Y" => paper
    case "C" | "Z" => scissors
  }

  implicit def toRPS(e: RPSEnum.Object): RPSValue = e match {
    case Rock       => rock
    case Paper      => paper
    case Scissors   => scissors
  }

  using(resource("src/main/resources/day2.txt")) { input => {

    val result = input.getLines
      .map(line => {
        val Array(theirs, b) = line.split(" ")

        // scala will auto-magically convert our input into the appropriate
        // values for us to extract what we need. Really makes this matching
        // simple and to the point
        val mine = b match {
          case "X" => theirs.win
          case "Y" => theirs.label
          case "Z" => theirs.loss
        }

        mine.play(theirs)
      })
      .sum

    println(result)
  }}
}
