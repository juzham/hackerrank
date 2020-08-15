package hackerrank.hashmaps

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import cats.instances.char

// https://www.hackerrank.com/challenges/sherlock-and-anagrams
object SherlockAndAnagrams {

  @annotation.tailrec
  def factorial(n: Int, result: BigInt = 1): BigInt = {
    if (n == 0)
      result
    else
      factorial(n - 1, result * n)
  }

  def binomialCoefficient(n: Int, k: Int): BigInt = {
    factorial(n) / (factorial(k) * factorial(n - k))
  }

  def calcAnagrams(chunks: Int, input: Array[Char]): Int = {
    input
      .sliding(chunks)
      .toArray
      .map(_.sorted.foldLeft("")(_ + _))
      .groupBy(s => s)
      .mapValues(_.size)
      .filter { case (_, v) => v > 1 }
      .mapValues(binomialCoefficient(_, 2).toInt)
      .foldLeft(0) { case (s, (_, v)) => s + v }
  }

  // Complete the sherlockAndAnagrams function below.
  def sherlockAndAnagrams(s: String): Int = {
    val chars = s.toCharArray()
    val charsSize = chars.size
    @annotation.tailrec
    def loop(i: Int, sumAnagrams: Int): Int = {
      if (i == charsSize)
        sumAnagrams
      else {
        loop(i + 1, sumAnagrams + calcAnagrams(i, chars))
      }
    }
    loop(1, 0)
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val q = stdin.readLine.trim.toInt

    for (qItr <- 1 to q) {
      val s = stdin.readLine

      val result = sherlockAndAnagrams(s)

      printWriter.println(result)
    }

    printWriter.close()
  }
}
