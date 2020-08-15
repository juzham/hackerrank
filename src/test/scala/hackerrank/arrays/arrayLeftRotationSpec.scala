package hackerrank

import org.specs2.mutable.Specification

class arrayLeftRotationSpec extends Specification {

  "rotLeft" >> {

    "it remains the same if rotated by 0" >> {
      val input    = Array(1, 2, 3, 4, 5)
      Solution8.rotLeft(input, 0) must beEqualTo(input)
    }

    "it should rotate left 1" >> {
      val input    = Array(1, 2, 3, 4, 5)
      val expected = Array(2, 3, 4, 5, 1)
      Solution8.rotLeft(input, 1) must beEqualTo(expected)
    }

    "it should rotate left 2" >> {
      val input    = Array(1, 2, 3, 4, 5)
      val expected = Array(3, 4, 5, 1, 2)
      Solution8.rotLeft(input, 2) must beEqualTo(expected)
    }

    "it should rotate left 3" >> {
      val input    = Array(1, 2, 3, 4, 5)
      val expected = Array(4, 5, 1, 2, 3)
      Solution8.rotLeft(input, 3) must beEqualTo(expected)
    }

    "it can rotate array left for max d" >> {
      val input    = Array(1, 2, 3, 4, 5)
      val expected = Array(5, 1, 2, 3, 4)
      Solution8.rotLeft(input, 4) must beEqualTo(expected)
    }

  }
}
