package hackerrank

import java.io._
import java.math._
import java.security._
import java.text._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

// https://www.hackerrank.com/challenges/ctci-ransom-note/problem
object RandsomNote {

  def combine(xs: Map[String, Int], ys: Map[String, Int]): Map[String, Int] = {
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) {
        case (my, (k, x)) =>
          my.updated(k, x + my.getOrElse(k, 0))
      }
    } else {
      ys.foldLeft(xs) {
        case (mx, (k, x)) =>
          mx.updated(k, x + mx.getOrElse(k, 0))
      }
    }
  }

  // Complete the checkMagazine function below.
  def checkMagazine(magazine: Array[String], note: Array[String]): String = {
    val magRef = magazine.foldLeft(Map[String, Int]()) {
      case (s, k) => combine(s, Map(k -> 1))
    }
    val noteRef = note.foldLeft(Map[String, Int]()) {
      case (s, k) => combine(s, Map(k -> 1))
    }
    val remainder = noteRef.dropWhile(i => { magRef.getOrElse(i._1, 0) >= i._2 })

    if(remainder.size > 0) "No" else "Yes"
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val mn = stdin.readLine.split(" ")

    val m = mn(0).trim.toInt

    val n = mn(1).trim.toInt

    val magazine = stdin.readLine.split(" ")

    val note = stdin.readLine.split(" ")
    println(checkMagazine(magazine, note))
  }
}
