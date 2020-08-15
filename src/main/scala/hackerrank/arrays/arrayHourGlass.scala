package hackerrank

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

// https://www.hackerrank.com/challenges/2d-array
object ArrayHourGlass {

  def hourglassSum(arr: Array[Array[Int]]): Int =
    (
      for (
        r <- 0 to 3;
        c <- 0 to 3
      ) yield (calcHourGlassSum(r, c, arr))
    ).max

  def calcHourGlassSum(rowOffset: Int, colOffSet: Int, arr: Array[Array[Int]]): Int = {
    arr(rowOffset + 0)(colOffSet + 0) +
      arr(rowOffset + 0)(colOffSet + 1) +
      arr(rowOffset + 0)(colOffSet + 2) +
      arr(rowOffset + 1)(colOffSet + 1) +
      arr(rowOffset + 2)(colOffSet + 0) +
      arr(rowOffset + 2)(colOffSet + 1) +
      arr(rowOffset + 2)(colOffSet + 2)
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val arr = Array.ofDim[Int](6, 6)

    for (i <- 0 until 6) {
      arr(i) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    val result = hourglassSum(arr)

    printWriter.println(result)

    printWriter.close()
  }
}
