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
object CountTriplets {

  def toMapIndex(arr: Array[Long]): Map[Long, List[Int]] = {
    val init = Map.empty[Long, List[Int]]
    arr.zipWithIndex
      .foldLeft(init) {
        case ((m, (v, i))) => {
          val newIs = m.get(v) match {
            case Some(is) => is :+ i
            case None     => List(i)
          }
          m + (v -> newIs)
        }
      }
  }
  // Complete the countTriplets function below.
  def countTriplets(arr: Array[Long], r: Long): Long = {
    val lookup = toMapIndex(arr)
    def loop(value: Long, valueIndex: Int, toFind: Int): Int = {
      if (toFind == 0)
        1
      else {
        val nextValue = value * r
        val maybeNextIs = lookup.get(nextValue)
        maybeNextIs match {
          case Some(nextIs) =>
            nextIs
              .map(nextI =>
                if (nextI > valueIndex)
                  loop(nextValue, nextI, toFind - 1)
                else
                  0
              )
              .sum
          case None => 0
        }
      }
    }
    arr.zipWithIndex.map { case (v, i) =>
      loop(v, i, 2)
    }.sum
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
