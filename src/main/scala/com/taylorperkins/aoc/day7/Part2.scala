package com.taylorperkins.aoc.day7
import com.taylorperkins.aoc.com.taylorperkins.aoc.core.Utils

import scala.annotation.tailrec
import scala.collection.mutable

object Part2 extends App
  with Utils
{

  case class DirectoryTree(name: String, parent: Option[DirectoryTree] = None)
  {
    val directories = mutable.Map.empty[String, Option[DirectoryTree]]
    val files = mutable.Set.empty[Tuple2[String, Int]]

    def addChild(childName: String): Unit =
      directories(childName) = Some(DirectoryTree(childName, parent=Some(this)))

    def addValue(value: Tuple2[String, Int]): Unit =
      files += value

    def getChild(childName: String): Option[DirectoryTree] =
      directories getOrElse(childName, {
        addChild(childName)
        directories(childName)
      })

    def top: DirectoryTree =
      parent match {
        case None => this
        case Some(tree) => tree.top
      }

    def fileSize: Int = files.map(_._2).sum

    def totalSize: Int =
      if (directories.isEmpty) fileSize
      else directories.values.map(_.get.totalSize).sum + fileSize

    def filterBySize(limit: Int): Iterable[Tuple2[Option[DirectoryTree], Int]] = {
      val s = totalSize
      val acc = if (s < limit) List(Tuple2(Some(this), s))
      else List.empty[Tuple2[Option[DirectoryTree], Int]]

      directories.values.flatMap(_.get.filterBySize(limit)) ++ acc
    }

    def local: String = s"$name/"

    def pwd: String =
      parent match {
        case Some(tree) => tree.pwd + local
        case None       => name
      }

    def dirSpace: List[Tuple2[Int, String]] =
      List(Tuple2(totalSize, pwd)) ++
        directories.values.flatMap(_.get.dirSpace)
  }


  // goal here is to return an Iterator of all the files+memory in the filesystem
  // as the full path. Can parse later
  def runCommands(commands: List[String]): DirectoryTree = {

    val pathRE = "\\$ cd (/.*)".r
    val cdRE = "\\$ cd (\\w+)".r
    val upRE = "\\$ cd \\.\\.".r
    val lsRE = "\\$ ls".r
    val fileRE = "(\\d+) (.*)".r
    val dirRE = "dir (.*)".r

    @tailrec
    def inner(tree: Option[DirectoryTree], commands: List[String]): Option[DirectoryTree] = {
      if (commands.isEmpty) tree
      else tree match {
        case None    => throw new Error("Not expecting to handle None")
        case Some(t) => commands.head match {
          // really don't do anything here
          case pathRE(dir) => inner(tree, commands.tail)

          // move down into the tree
          case cdRE(dir) => inner(t.getChild(dir), commands.tail)

          // move to the parent
          case upRE() => inner(t.parent, commands.tail)

          // really don't do anything here
          case lsRE() => inner(tree, commands.tail)

          // add the file+mem to the current node in the tree
          case fileRE(fileSize, name) => {
            t.addValue(Tuple2(name, fileSize.toInt))
            inner(tree, commands.tail)
          }

          // really don't do anything here
          case dirRE(name) => inner(tree, commands.tail)

          case _ => throw new Error(s"Bad pattern for ${commands.head}")
        }
      }
    }

    // Kind of cheating that I start with the root path, but w/e

    inner(Some(DirectoryTree("/")), commands) match {
      case Some(tree) => tree.top
      case None       => throw new Error("You suck at this.")
    }
  }

  using(resource("src/main/resources/day7.txt")) { input => {
    val tree = runCommands(input.getLines.toList)

    val fsSize = 70000000
    val updateSize = 30000000

    val unusedSpace = fsSize - tree.totalSize
    val requiredSize = updateSize - unusedSpace

    println(requiredSize)

//    tree.dirSpace.sorted.foreach(record => println(s"${record._1} -> ${record._2}"))
    println(tree.dirSpace.sorted.find(_._1 >= requiredSize))
  }}
}
