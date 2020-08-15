package hackerrank

import java.io._
import java.math._
import java.security._
import java.text._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.math.max
import scala.collection.immutable._

// https://www.hackerrank.com/challenges/crush/problem
object ArrayManipulation {

  // pretty much copied from the cats library for monoid instance
  def combine(xs: SortedMap[Int, Long], ys: SortedMap[Int, Long]): SortedMap[Int, Long] = {
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) {
        case (my, (k, x)) =>
          my.updated(k, x + my.getOrElse(k, 0L))
      }
    } else {
      ys.foldLeft(xs) {
        case (mx, (k, x)) =>
          mx.updated(k, x + mx.getOrElse(k, 0L))
      }
    }
  }

  def flatten(n: Int, queries: Array[Array[Int]]): SortedMap[Int, Long] = {
    queries.foldLeft((SortedMap.empty[Int, Long])) {
      case (s, Array(a, b, k)) => {
        combine(s, SortedMap((a -> k), (b + 1 -> -k)))
      }
    }
  }

  def arrayManipulation(n: Int, queries: Array[Array[Int]]): Long = {
    flatten(n, queries).foldLeft((0L, 0L)) {
      case ((maxS, s), (_, v)) => {
        val newS = s + v
        (max(maxS, newS), newS)
      }
    }._1
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val nm = stdin.readLine.split(" ")

    val n = nm(0).trim.toInt

    val m = nm(1).trim.toInt

    val queries = Array.ofDim[Int](m, 3)

    for (i <- 0 until m) {
      queries(i) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    val result = arrayManipulation(n, queries)

    printWriter.println(result)

    printWriter.close()
  }
}
