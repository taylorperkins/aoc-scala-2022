package com.taylorperkins.aoc.day13
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.annotation.tailrec
import scala.language.implicitConversions

object Part2 extends App
  with Utils
{

  implicit class StringUtils(string: String)
  {
    implicit def toInputArray: Array[Any] = {
      @tailrec
      def toArr0(acc: Array[Any]): Array[Any] = {
        if (acc.length == 2) Array.empty[Any]
        else if (acc.count(_ == '[') == 1) acc.tail.dropRight(1)
        else
          acc.reverse.indexOf('[') match
            // no more
            case -1 => acc

            // we have some more left to parse
            case idx =>
              val startIdx = acc.length - idx - 1
              // we know there should be a closing ']'
              val endIdx = acc.slice(startIdx+1, acc.length).indexOf(']') + startIdx + 1

              val beg = acc.slice(0, startIdx)
              val mid = acc.slice(startIdx+1, endIdx)
              val end = acc.slice(endIdx+1, acc.length)

              val combined = (beg :+ mid) ++ end

              // slice it excluding our characters
              toArr0(combined)
      }

      def parse(input: String): Array[Any] = {
        @tailrec
        def parse0(input: String, acc: Array[Any]): Array[Any] = {
          if (input.isEmpty) acc
          else if (input.head == ',') parse0(input.tail, acc)
          else if (input.head == ']' || input.head == '[') parse0(input.tail, acc :+ input.head)
          else {
            val nextNonNum = input.indexWhere(List(',', ']', '[').contains(_))
            parse0(input.slice(nextNonNum, input.length), acc :+ input.slice(0, nextNonNum).toInt)
          }
        }

        parse0(input, Array.empty[Any])
      }

      toArr0(parse(string.replace(" ", "")))
    }
  }


  def order(left: Array[Any], right: Array[Any]): Boolean = {

    def compare(left: Any, right: Any): Option[Boolean] =
    {
      (left, right) match {
        case (l: Array[Any],  r: Array[Any] ) => compareArr(l, r)
        case (l: Int,         r: Array[Any] ) => compareArr(Array(l), r)
        case (l: Array[Any],  r: Int        ) => compareArr(l, Array(r))
        case (l: Int,         r: Int        ) =>
          if (l < r) Some(true)
          else if (l > r) Some(false)
          else None
      }
    }

    @tailrec
    def compareArr(left: Array[Any], right: Array[Any]): Option[Boolean] = {
      // left ran out "first"
      if (left.isEmpty && !right.isEmpty) Some(true)
      // inverse
      else if (!left.isEmpty && right.isEmpty) Some(false)
      // can't determine if in order
      else if (left.isEmpty && right.isEmpty) None
      else compare(left.head, right.head) match
        // can't determine if in order, next
        case None => compareArr(left.tail, right.tail)
        case some => some
    }

    compareArr(left, right).get
  }


  time {
    // around 190ms
    using(resource("src/main/resources/day13.txt")) { input => {
      val packets: Array[Array[Any]] = input.toAOC
        .split("\n\n")
        .flatMap(_.split("\n").map(_.toInputArray))

      val dividerPackets: Array[Array[Any]] = Array("[[2]]", "[[6]]")
        .map(_.toInputArray)

      val result = (packets ++ dividerPackets)
        .zipWithIndex
        .sortWith((left, right) => order(left._1, right._1))
        .zipWithIndex
        .filter(_._1._2 >= packets.length)
        .map(_._2+1)
        .product

      println(result)
    }}
  }
}
