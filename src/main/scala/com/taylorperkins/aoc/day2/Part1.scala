package com.taylorperkins.aoc.day2
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils



object Part1 extends App
  with Utils
{
  object RPSEnum extends Enumeration {
    type Object = Value
    val Rock, Paper, Scissors = Value
  }

  case class RPSValue(label: RPSEnum.Object, value: Int, win: RPSEnum.Object, loss: RPSEnum.Object) {
    def play(other: RPSValue): Int = other.label match
      case `label`   => 3 + value
      case `win`     => 6 + value
      case `loss`    => 0 + value
  }

  val rock = RPSValue(RPSEnum.Rock, 1, RPSEnum.Scissors, RPSEnum.Paper)
  val paper = RPSValue(RPSEnum.Paper, 2, RPSEnum.Rock, RPSEnum.Scissors)
  val scissors = RPSValue(RPSEnum.Scissors, 3, RPSEnum.Paper, RPSEnum.Rock)

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
        val Array(a, b) = line.split(" ")
        b.play(a)
      })
      .sum

    println(result)
  }}
}
