package com.taylorperkins.aoc.day10
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

object Part1 extends App
  with Utils
{

  // the "cycle" is a constant that can be managed by another system, like a list
  // where each value represents a cycle.
  case class ClockCircuit(xRegister: Int = 1)
  {
    def +(value: Int): ClockCircuit = ClockCircuit(xRegister + value)
  }

  trait Command
  {
    // a command can take up many cycles (blocking)
    // the size of the list determines the number of cycles it took to complete
    def apply(clockCircuit: ClockCircuit): List[ClockCircuit]

    // anticipating pre and post commands
    def pre(clockCircuit: ClockCircuit): ClockCircuit = clockCircuit
    def post(clockCircuit: ClockCircuit): ClockCircuit = clockCircuit
  }

  object Noop extends Command
  {
    // doesn't really do anything, just adds one to the cycle
    override def apply(clockCircuit: ClockCircuit): List[ClockCircuit] = List(clockCircuit)
  }

  case class AddX(value: Int) extends Command
  {
    override def apply(clockCircuit: ClockCircuit): List[ClockCircuit] = List(clockCircuit, clockCircuit + value)
  }


  object Cycle
  {
    def apply(commands: List[Command]): List[ClockCircuit] = {
      def inner(commands: List[Command], acc: List[ClockCircuit]): List[ClockCircuit] = {
        if (commands.isEmpty) acc
        else inner(commands.tail, acc ++ commands.head(acc.last))
      }

      // start with an empty circuit
      inner(commands, List(ClockCircuit()))
    }
  }

  using(resource("src/main/resources/day10.txt")) { input => {

    val addxRE = "addx (.*)".r
    val commands = input.getLines.map(line => {
      line match {
        case "noop" => Noop
        case addxRE(value) => AddX(value.toInt)
      }
    }).toList

    // dropping the initial state
    val cycles = Cycle(commands)

    // instructions say "up to" the twentieth cycle
    val result = (19 to cycles.size by 40)
      .map(cycle => cycles(cycle).xRegister * (cycle+1))
      .sum

    println(result)

  }}
}
