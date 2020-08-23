package hackerrank.hashmaps

import org.specs2.mutable.Specification

class CountTripletsSpec extends Specification {
  "countTriplet" >> {
    "should a triplet when it exists" >> {
      val state = State(Map(2L -> 1L, 4L -> 1L))
      val value = 1L
      val ratio = 2L
      CountTriplets.countTriplet(state, value, ratio) must beEqualTo(1)
    }

    "should return 0 if triplet doesn't exit" >> {
      val state = State(Map(2L -> 1L, 4L -> 1L))
      val value = 1L
      val ratio = 10L
      CountTriplets.countTriplet(state, value, ratio) must beEqualTo(0)
    }

    "should count multiple triplets" >> {
      val state = State(Map(2L -> 2L, 4L -> 2L))
      val value = 1L
      val ratio = 2L
      CountTriplets.countTriplet(state, value, ratio) must beEqualTo(4)
    }

    "should count ratio of 1 correctly" >> {
      val state = State(Map(1L -> 2L))
      val value = 1L
      val ratio = 1L
      CountTriplets.countTriplet(state, value, ratio) must beEqualTo(1)
    }

    "should count ratio of 1 len 3 correctly" >> {
      val state = State(Map(1L -> 3L))
      val value = 1L
      val ratio = 1L
      CountTriplets.countTriplet(state, value, ratio) must beEqualTo(3)
    }

    "should count ratio of 1 len 4 correctly" >> {
      val state = State(Map(1L -> 4L))
      val value = 1L
      val ratio = 1L
      CountTriplets.countTriplet(state, value, ratio) must beEqualTo(6)
    }
  }

  "incState" >> {
    "should update an exiting key" >> {
      val state = State(Map(1L -> 1L, 2L -> 1L))
      val key = 1L
      val expectedState = State(Map(1L -> 2L, 2L -> 1L))
      CountTriplets.incState(state, key) must beEqualTo(expectedState)
    }

    "should add a new key if none exists" >> {
      val state = State(Map(2L -> 1L))
      val key = 1L
      val expectedState = State(Map(1L -> 1L, 2L -> 1L))
      CountTriplets.incState(state, key) must beEqualTo(expectedState)
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

    "should calc out of order array" >> {
      val input: Array[Long] = Array(1, 2, 1, 2, 4)
      val ratio = 2
      CountTriplets.countTriplets(input, ratio) must beEqualTo(3)
    }

    "should count by a ratio of 1 and len 3" >> {
      val input: Array[Long] = Array(1, 1, 1)
      val ratio = 1
      CountTriplets.countTriplets(input, ratio) must beEqualTo(1)
    }

    "should count by a ratio of 1 and len 4" >> {
      val input: Array[Long] = Array(1, 1, 1, 1)
      val ratio = 1
      CountTriplets.countTriplets(input, ratio) must beEqualTo(4)
    }

    "should work with tricky order" >> {
      val input: Array[Long] = Array(1, 4, 1, 2, 4)
      val ratio = 2
      CountTriplets.countTriplets(input, ratio) must beEqualTo(2)
    }

    "should count large single combinations" >> {
      val input: Array[Long] = Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
      val ratio = 1
      CountTriplets.countTriplets(input, ratio) must beEqualTo(161700)
    }
  }
}
