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

  def hourglassSum(arr: Array[Array[Int]]): Int = {
    var maxHourGlassSum = 0
    for (r <- 0 to 3) {
      for (c <- 0 to 3) {
        val currSum = calcHourGlassSum(r, c, arr)
        if (currSum > maxHourGlassSum) maxHourGlassSum = currSum
      }
    }
    maxHourGlassSum
  }

  def calcHourGlassSum(rowOffset: Int, colOffSet: Int, arr: Array[Array[Int]]): Int = {
    arr(rowOffset + 0)(colOffSet + 0) +
      arr(rowOffset + 0)(colOffSet + 1) +
      arr(rowOffset + 0)(colOffSet + 2) +
      arr(rowOffset + 1)(colOffSet + 1) +
      arr(rowOffset + 2)(colOffSet + 0) +
      arr(rowOffset + 2)(colOffSet + 1) +
      arr(rowOffset + 2)(colOffSet + 2)
  }

  def hourglassSumOldv1(arr: Array[Array[Int]]): Int = {
    arr
      .map(_.sliding(3).toArray)
      .transpose
      .map(_.sliding(3).toArray)
      .flatten
      .map(sumSingleHourGlass(_))
      .max
  }

  def hourglassSumOld(arr: Array[Array[Int]]): Int = {
    (0 to 3)
      .foldLeft(Array.emptyIntArray)((sum, r) => {
        val row0 = arr(r).sliding(3).toArray
        val row1 = arr(r + 1).sliding(3).toArray
        val row2 = arr(r + 2).sliding(3).toArray

        val groupMax = (0 to 3)
          .foldLeft(Array.emptyIntArray)((subSum, i) => {
            subSum :+ sumSingleHourGlass(Array(row0(i), row1(i), row2(i)))
          })
          .max
        sum :+ groupMax
      })
      .max
  }

  def sumSingleHourGlass(arr: Array[Array[Int]]): Int = {
    arr.flatten.zipWithIndex.foldLeft(0)((sum, i) => {
      val value = i._1
      val index = i._2
      sum + (if (index == 3 || index == 5) 0 else value)
    })
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
