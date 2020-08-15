package hackerrank

import org.specs2.mutable.Specification

class ArrayHourGlassSpec extends Specification {

  "calcHourGlassSum" >> {
    "it should calc the hour glass on col offset" >> {
      val input =
        Array(
          Array(0, 1, 1, 2),
          Array(0, 10, 1, 10),
          Array(0, 1, 1, 1)
        )
      ArrayHourGlass.calcHourGlassSum(0, 1, input) must beEqualTo(8)
    }

    "it should calc the hour glass on row offset" >> {
      val input =
        Array(
          Array(0, 0, 0, 0),
          Array(0, 1, 1, 1),
          Array(0, 10, 1, 10),
          Array(0, 1, 1, 1)
        )
      ArrayHourGlass.calcHourGlassSum(1, 1, input) must beEqualTo(7)
    }
  }

  "hourglassSum" >> {
    "it should return max of all hour glasses in a simple case" >> {
      val input =
        Array(
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 1, 1, 1)
        )
      ArrayHourGlass.hourglassSum(input) must beEqualTo(7)
    }

    "it should return max of all hour glasses in a more complex case" >> {
      val input =
        Array(
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 1, 1, 1),
          Array(1, 1, 1, 2, 2, 2),
          Array(1, 1, 1, 1, 2, 1),
          Array(1, 1, 1, 2, 2, 2)
        )
      ArrayHourGlass.hourglassSum(input) must beEqualTo(14)
    }
  }
}
