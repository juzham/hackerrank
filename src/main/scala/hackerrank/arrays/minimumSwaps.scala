package hackerrank

import java.io._
import java.math._
import java.security._
import java.text._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

// https://www.hackerrank.com/challenges/minimum-swaps-2
object MinimumSwaps {

  def swapItems(toSort: Array[Int], currentItem: (Int, Int)): Array[Int] = {
    val (value, index) = currentItem
    val displacedValue = toSort(value - 1)
    toSort.update(value - 1, value)
    toSort.update(index, displacedValue)
    toSort
  }

  // Complete the minimumSwaps function below.
  def minimumSwaps(arr: Array[Int]): Int = {
    val maxi = arr.size
    @annotation.tailrec
    def loop(i: Int, toSort: Array[Int], swaps: Int): Int = {
      if (i >= maxi)
        swaps
      else {
        val value = toSort(i)
        val (newi, newToSort, newSwaps) =
          if (value == i + 1)
            (i + 1, toSort, swaps)
          else
            (i, swapItems(toSort, (value, i)), swaps + 1)

        loop(newi, newToSort, newSwaps)
      }
    }
    loop(0, arr, 0)
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val arr = stdin.readLine.split(" ").map(_.trim.toInt)
    val res = minimumSwaps(arr)

    printWriter.println(res)

    printWriter.close()
  }
}
