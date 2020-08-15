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

// #2
object Valley {

    // Complete the countingValleys function below.
    def countingValleys(n: Int, s: String): Int = {
      case class Record(altitude: Int, valleys: Int)
      s.toCharArray().foldLeft(Record(0, 0))((record, directionCode) => {
        directionCode match {
          case 'U' => Record(record.altitude + 1, record.valleys)
          case 'D' => if(record.altitude == 0){
            Record(record.altitude - 1, record.valleys + 1)
          } else {
            Record(record.altitude - 1, record.valleys)
          }
        }
      }).valleys
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val n = stdin.readLine.trim.toInt

        val s = stdin.readLine

        val result = countingValleys(n, s)

        printWriter.println(result)

        printWriter.close()
    }
}