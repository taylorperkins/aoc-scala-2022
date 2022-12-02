package com.taylorperkins.aoc.day2
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils



object Part1 extends App
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

  implicit def toRPS(s: String): RPSValue = s match {
    case "A" | "X" => rock
    case "B" | "Y" => paper
    case "C" | "Z" => scissors
  }

  // Some sanity checks
  //  for {
  //    left <- Array("A", "B", "C")
  //    right <- Array("X", "Y", "Z")
  //  } println(left.play(right))

  using(resource("src/main/resources/day2.txt")) { input => {

    val result = input.getLines
      .map(line => {
        val Array(theirs, mine) = line.split(" ")
        mine.play(theirs)
      })
      .sum

    println(result)
  }}
}
