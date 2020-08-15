package hackerrank

import java.io._
import java.math._
import java.security._
import java.text._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

import scala.math.{max, min}

// https://www.hackerrank.com/challenges/new-year-chaos
object MinBrides {
  val maxBribes: Int = 2

  def findThoseBribed(seen: Map[Int, Boolean], bribed: Map[Int, Boolean], index: Int): Map[Int, Boolean] = {
    (1 to maxBribes + 1).foldLeft(bribed) {
      case (b, i) => {
        val potential = index - i
        if (potential >= 0 && !seen.contains(potential)) b + (potential -> true) else b
      }
    } - index
  }

  def calcBribes(bribed: Map[Int, Boolean], index: Int): Int = {
    bribed.filter(b => b._1 < index).size
  }

  // Complete the minimumBribes function below.
  def minimumBribes(q: Array[Int]): Any = {
    val maxi = q.size - 1
    @annotation.tailrec
    def loop(
        i: Int,
        seen: Map[Int, Boolean],
        bribed: Map[Int, Boolean],
        chaos: Boolean,
        totalBribes: Int
    ): (Boolean, Int) = {
      if (chaos || i > maxi)
        (chaos, totalBribes)
      else {
        val personIndex = q(i) - 1
        val newBribed = findThoseBribed(seen, bribed, personIndex)
        val newBribes = calcBribes(newBribed, personIndex)
        loop(i + 1, seen + (personIndex -> true), newBribed, newBribes > maxBribes, totalBribes + newBribes)
      }
    }

    loop(0, Map(), Map(), false, 0) match {
      case (true, _)       => "Too chaotic"
      case (false, bribes) => bribes
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val t = stdin.readLine.trim.toInt

    for (tItr <- 1 to t) {
      val n = stdin.readLine.trim.toInt

      val q = stdin.readLine.split(" ").map(_.trim.toInt)
      minimumBribes(q)
    }
  }
}
