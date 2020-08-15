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

// #3
object JumpingOnClouds {

    // Complete the jumpingOnClouds function below.
    def jumpingOnClouds(c: Array[Int]): Int = {
        val maxPosition = c.length - 1
        val smallJump = 1
        val bigJump = 2
        val safeCloud = 0
        def loop(position: Int, jumps: Int): Int = {
            if(position >= maxPosition) {
                jumps
            } else {
                val jumpOption =
                    if(position + bigJump <= maxPosition && c(position + bigJump) == safeCloud) {
                        bigJump
                    } else {
                        smallJump
                    }
                loop(position + jumpOption, jumps + 1)
            }
        }
        loop(0, 0)
    }

    def main(args: Array[String]) {
        val stdin = scala.io.StdIn

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val n = stdin.readLine.trim.toInt

        val c = stdin.readLine.split(" ").map(_.trim.toInt)
        val result = jumpingOnClouds(c)

        printWriter.println(result)

        printWriter.close()
    }
}