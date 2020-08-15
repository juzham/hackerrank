package hackerrank.hashmaps

// https://www.hackerrank.com/challenges/count-triplets-1/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=dictionaries-hashmaps
import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
import scala.collection.parallel.immutable._
import scala.collection.parallel.mutable._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._

// https://www.hackerrank.com/challenges/count-triplets-1
object CountTriplets {

    // Complete the countTriplets function below.
    def countTriplets(arr: Array[Long], r: Long): Long = {
      ???

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
