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
import collection.immutable.Stream

// # 4-6
object RepeatingString {

    // Complete the repeatedString function below.
    def repeatedString(s: String, n: Long): Long = {
      val searchChar = 'a'
      val strings = s.toVector
      val lenStrings = strings.length
      var searchCharCount: Long = 0L
      var i = 0
      while(i < n) {
        val index = i % lenStrings
        if(strings(index.toInt) == searchChar) searchCharCount += 1
        i += 1
      }
      searchCharCount
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val s = stdin.readLine

        val n = stdin.readLine.trim.toLong

        val result = repeatedString(s, n)

        printWriter.println(result)

        printWriter.close()
    }
}