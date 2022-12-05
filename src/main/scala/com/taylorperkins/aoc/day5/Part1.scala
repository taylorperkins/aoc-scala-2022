package com.taylorperkins.aoc.day5
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Stack

object Part1 extends App
  with Utils
{

  class Supplies(size: Int)
  {
      val stacks = Array.fill(size) { new mutable.Stack[String].empty }

      def push(idx: Int, value: String): Unit =
        if (!value.isEmpty) {
          stacks(idx).push(value)
        }

      def pop(idx: Int): String = stacks(idx).pop()

      def move(amount: Int, from: Int, to: Int): Unit =
        for {
          n <- 1 to amount
        } push(to, pop(from))

      def top: String = stacks.map(_.head).mkString
  }


  @tailrec
  def toSupplies(line: String, supplies: Supplies, idx: Int = 0): Unit = {
    if (!line.isEmpty) {
      val next = line.take(3).replace("[", "").replace("]", "")
      if (!next.trim().isEmpty) {
        supplies.push(idx, next)
      }
      val drop = if (line.length == 3) 3 else 4
      toSupplies(line.drop(drop), supplies, idx+1)
    }
  }


  using(resource("src/main/resources/day5.txt")) { input => {

    val Array(inputStacks, inputCommands) = input.toAOC.split("\n\n")

    val rawStacks = inputStacks.split("\n").reverse.drop(1)

    val supplies = Supplies(rawStacks.head.split(' ').length)
    rawStacks.foreach(toSupplies(_, supplies))

    val commandRE = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
    inputCommands.split("\n")
      .foreach(line => {
        line match {
          case commandRE(amount, from, to) => {
            println(mutable.Seq(amount, from, to))
            supplies.move(amount.toInt, from.toInt - 1, to.toInt - 1)
          }
        }
      })

    println(supplies.top)
  }}
}
