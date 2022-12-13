package com.taylorperkins.aoc.day11
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.collection.mutable

object Part2 extends App
  with Utils
{

  implicit class MathUtils(value: String)
  {
    def toOperand: (BigInt, BigInt) => BigInt  = value match {
      case "+" => _ + _
      case "-" => _ - _
      case "*" => _ * _
      case "/" => _ / _
    }
  }


  class KeepAwayGame(val monkeys: Map[Int, Monkey])
  {
    val div: BigInt = monkeys.values.map(_.test).product

    def turn(monkey: Monkey): Unit = {
      monkey.toss(div) match
        case Some(Tuple2(monkeyId, wl)) =>
          // monkeys only inspect tossed items
          monkeys(monkeyId).receive(wl)
          turn(monkey)

        case None => ()
    }

    def round(): Unit =
      monkeys.values.toList
        .sorted
        .foreach(monkey => {
//          println(s"Monkey ${monkey.id}:")
          turn(monkey)
        })

    def inspectionMap: Map[Int, Int] = monkeys
      .values.map(monkey => (monkey.id, monkey.inspections))
      .toMap
  }

  trait Monkey extends Ordered[Monkey] {
    override def compare(that: Monkey): Int = this.id compare that.id

    val id: Int
    val items: mutable.Queue[BigInt]
    val test: BigInt

    var inspections = 0

    def toss(div: BigInt): Option[(Int, BigInt)]

    def operation(wl: BigInt): BigInt
    def leftTerm(wl: BigInt): BigInt
    def rightTerm(wl: BigInt): BigInt

    def operand: (BigInt, BigInt) => BigInt

    def receive(value: BigInt): Unit = items.enqueue(value)

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
        .map(value => BigInt(value.toInt))
        .toList

      val Array(left, op, right) = input(2)
        .replace("  Operation: new = ", "")
        .split(" ")

      val opFn: String => BigInt => BigInt = (str: String) => str match
        case "old" => (i: BigInt) => i
        case num => (i: BigInt) => num.toInt

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
        override val items: mutable.Queue[BigInt] = mutable.Queue(startingItems: _*)
        override val test: BigInt = BigInt(divBy)

        override def operation(wl: BigInt): BigInt = op.toOperand(leftTerm(wl), rightTerm(wl))
        override def leftTerm(wl: BigInt): BigInt = opFn(left)(wl)
        override def rightTerm(wl: BigInt): BigInt = opFn(right)(wl)

        override def operand: (BigInt, BigInt) => BigInt = op.toOperand

        override def toss(div: BigInt): Option[(Int, BigInt)] = {
          if (items.isEmpty) None
          else {
            var wl = items.dequeue
            inspections += 1
//            println(s"\tMonkey inspects an item with a worry level of $wl.")

            wl = operation(wl)
//            println(s"\t\tNew wl: $wl.")

            wl %= div
//            println(s"\t\tMonkey gets bored with item. Worry level is divided by 3 to $wl.")

            val isDiv = wl % test == 0
            val monkeyId = if (isDiv) trueId else falseId
//            println(s"\t\tItem with worry level $wl is thrown to monkey $monkeyId.")
            Some(Tuple2(monkeyId, wl))
          }
        }
    }
  }

  time {
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
        i <- 1 to 10000
      } {
        kag.round()
//        if ((List(1, 20) ++ (1000 to 10000 by 1000)) contains (i)) {
//          println(i)
//          println(kag.inspectionMap)
//          println()
//        }
      }

      println(kag.inspectionMap)

      val result = kag.inspectionMap
        .values
        .toList
        .sorted
        .reverse
        .take(2)
        .map(BigInt(_))
        .reduce(_ * _)

      println(result)

    }
    }
  }
}
