package hackerrank.hashmaps

// https://www.hackerrank.com/challenges/count-triplets-1/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=dictionaries-hashmaps
import java.io._
import java.math._
import java.security._
import java.text._
// import java.util._
// import java.util.concurrent._
// import java.util.function._
// import java.util.regex._
// import java.util.stream._
import scala.collection.immutable._
// import scala.collection.mutable._
// import scala.collection.concurrent._
// import scala.collection.parallel.immutable._
// import scala.collection.parallel.mutable._
// import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._

// https://www.hackerrank.com/challenges/count-triplets-1

case class State(singles: Map[Long, Long], pairs: Map[(Long, Long), Long])
object CountTriplets {

  def factorial(n: Long, result: BigInt = 1): BigInt = {
    if (n == 0)
      result
    else
      factorial(n - 1, result * n)
  }

  def choose(n: Long, k: Int): BigInt = {
    factorial(n) / (factorial(k) * factorial(n - k))
  }

  def incState(state: State, key: Long, r: Long): State = {
    val keyRatio = key * r

    val newSingleValue = state.singles.get(key) match {
      case Some(value) => value + 1
      case None        => 1
    }

    val newPair = state.singles.get(keyRatio) match {
      case Some(ratioValue) => Map((key, keyRatio) -> newSingleValue * ratioValue)
      case None             => Map.empty[(Long, Long), Long]
    }

    State(state.singles + (key -> newSingleValue), state.pairs ++ newPair)
  }

  def countTriplet(state: State, value: Long, r: Long): Long = {
    val iR1 = value * r
    val iR2 = iR1 * r
    state.singles.get(iR1) match {
      case Some(i1) =>
        if (r == 1)
          if (i1 > 1)
            choose(i1, 2).toLong
          else
            0
        else {
          state.singles.get(iR2) match {
            case Some(i2) => i1 * i2
            case None     => 0
          }
        }
      case None => 0
    }
  }

  // Complete the countTriplets function below.
  def countTriplets(arr: Array[Long], r: Long): Long = {
    val init = (0L, State(Map.empty, Map.empty))
    arr.reverse
      .foldLeft(init) {
        case ((sumTriplets, state), i) =>
          (sumTriplets + countTriplet(state, i, r), incState(state, i, r))
      }
      ._1
  }

  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val nr = StdIn.readLine.replaceAll("\\s+$", "").split(" ")

    val n = nr(0).toInt

    val r = nr(1).toLong

    val arr = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toLong)
    val ans = countTriplets(arr, r)

    printWriter.println(ans)

    printWriter.close()
  }
}
