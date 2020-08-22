package hackerrank.hashmaps

import org.specs2.mutable.Specification

class CountTripletsSpec extends Specification {
  "toMapIndex" >> {
    "should convert an array to a Map of indexes" >> {
      val input: Array[Long] = Array(1L, 2L, 2L, 4L)
      val expected: Map[Long, List[Int]] = Map(
        1L -> List(0),
        2L -> List(1, 2),
        4L -> List(3)
      )
      CountTriplets.toMapIndex(input) must beEqualTo(expected)
    }
  }

  "countTriplets" >> {
    "should count simple triplets" >> {
      val input: Array[Long] = Array(1L, 2L, 4L)
      val ratio = 2
      CountTriplets.countTriplets(input, ratio) must beEqualTo(1)
    }

    "should count more complex triplets" >> {
      val input: Array[Long] = Array(1L, 2L, 2L, 4L)
      val ratio = 2
      CountTriplets.countTriplets(input, ratio) must beEqualTo(2)
    }

    "should count by a ratio of 3" >> {
      val input: Array[Long] = Array(1, 3, 9, 9, 27, 81)
      val ratio = 3
      CountTriplets.countTriplets(input, ratio) must beEqualTo(6)
    }
  }
}
