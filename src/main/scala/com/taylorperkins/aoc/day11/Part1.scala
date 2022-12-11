package com.taylorperkins.aoc.day11
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.collection.mutable

object Part1 extends App
  with Utils
{

  implicit class MathUtils(value: String)
  {
    def toOperand: (Int, Int) => Int  = value match {
      case "+" => _ + _
      case "-" => _ - _
      case "*" => _ * _
      case "/" => _ / _
    }
  }


  class KeepAwayGame(monkeys: Map[Int, Monkey])
  {

    val inspectionTracker: mutable.Queue[Int] = mutable.Queue.empty[Int]

    def turn(monkey: Monkey): Unit = {
      monkey.toss match
        case Some(Tuple2(monkeyId, wl)) =>
          // monkeys only inspect tossed items
          inspectionTracker.enqueue(monkey.id)
          monkeys(monkeyId).receive(wl)
          turn(monkey)

        case None => ()
    }

    def round(): Unit =
      monkeys.values.toList
        .sorted
        .foreach(monkey => {
          println(s"Monkey ${monkey.id}:")
          turn(monkey)
        })
  }

  trait Monkey extends Ordered[Monkey] {
    override def compare(that: Monkey): Int = this.id compare that.id

    val id: Int
    val items: mutable.Queue[Int]

    def toss: Option[(Int, Int)]

    def operation(wl: Int): Int

    def leftTerm(wl: Int): Int

    def rightTerm(wl: Int): Int

    def operand: (Int, Int) => Int

    def receive(value: Int): Unit = items.enqueue(value)
  }

  object Monkey
  {
    def fromInput(input: List[String]): Monkey = {
      val monkeyId = input.head
        .replace(":", "")
        .replace("Monkey ", "")
        .toInt

      val startingItems = input(1)
        .replace("  Starting items: ", "")
        .split(", ")
        .map(_.toInt)
        .toList

      val Array(left, op, right) = input(2)
        .replace("  Operation: new = ", "")
        .split(" ")

      val opFn: String => Int => Int = (str: String) => str match
        case "old" => (i: Int) => i
        case num => (i: Int) => num.toInt

      val divBy = input(3)
        .replace("  Test: divisible by ", "")
        .toInt

      val trueId = input(4)
        .replace("    If true: throw to monkey ", "")
        .toInt

      val falseId = input(5)
        .replace("    If false: throw to monkey ", "")
        .toInt

      new Monkey:
        override val id: Int = monkeyId
        override val items: mutable.Queue[Int] = mutable.Queue(startingItems: _*)

        override def operation(wl: Int): Int = op.toOperand(leftTerm(wl), rightTerm(wl))

        override def leftTerm(wl: Int): Int = opFn(left)(wl)

        override def rightTerm(wl: Int): Int = opFn(right)(wl)

        override def operand: (Int, Int) => Int = op.toOperand

        override def toss: Option[(Int, Int)] = {
          if (items.isEmpty) None
          else {
            val wl = items.dequeue
            println(s"\tMonkey inspects an item with a worry level of $wl.")
            val newWl = operation(wl)
            println(s"\t\tNew wl: $newWl.")
            val anotherWorryLevel = newWl / 3
            println(s"\t\tMonkey gets bored with item. Worry level is divided by 3 to $anotherWorryLevel.")
            val isDiv = anotherWorryLevel % divBy == 0
            val monkeyId = if (isDiv) trueId else falseId
            println(s"\t\tItem with worry level $anotherWorryLevel is thrown to monkey $monkeyId.")
            Some(Tuple2(monkeyId, anotherWorryLevel))
          }
        }
    }
  }


  using(resource("src/main/resources/day11.txt")) { input => {

    val monkeys = input.toAOC
      // each monkey's notes
      .split("\n\n")
      .map(notes => {
        val m = Monkey.fromInput(notes.split("\n").toList)
        (m.id, m)
      })
      .toMap

    val kag = new KeepAwayGame(monkeys)
    for {
      _ <- 1 to 20
    } kag.round()

    val monkeyInspectionMap = kag.inspectionTracker
      .groupBy(l => l)
      .map(group => Tuple2(group._1, group._2.size))

    val result = monkeyInspectionMap
      .values
      .toList
      .sorted
      .reverse
      .take(2).product

    println(result)


  }}
}
