package hackerrank

import org.specs2.mutable.Specification
import scala.collection.immutable.SortedMap

object ArrayManipulationSpec extends Specification {

  "flatten" >> {
    "should flatten and aggregate queries" >> {
      val n = 10
      val queries =
        Array(
          Array(1, 5, 3),
          Array(5, 8, 7),
          Array(6, 9, 1)
        )
      val expected =
        SortedMap(
          (1 -> 3),
          (5 -> 7),
          (6 -> -2),
          (9 -> -7),
          (10 -> -1)
        )

      ArrayManipulation.flatten(n, queries) must beEqualTo(expected)
    }
  }

  "arrayManipulation" >> {
    "should get the max for a simple input" >> {
      val n = 10
      val queries =
        Array(
          Array(1, 4, 3),
          Array(4, 9, 7),
          Array(6, 9, 1)
        )
      ArrayManipulation.arrayManipulation(n, queries) must beEqualTo(10)
    }
  }
}
