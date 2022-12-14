package com.taylorperkins.aoc
package com.taylorperkins.aoc.core

// required when using reflection, like `using` does
import java.io.FileNotFoundException
import scala.language.reflectiveCalls
import scala.io.{BufferedSource, Source}


trait Utils
{
  // from https://alvinalexander.com/scala/how-to-open-read-text-files-in-scala-cookbook-examples/
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def resource(fp: String): BufferedSource =
    Source.fromFile(fp)

  implicit class BufferedSourceUtils(s: BufferedSource)
  {
    def toAOC = s.getLines.mkString("\n")
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0)/1000000) + "ms")
    result
  }

}
