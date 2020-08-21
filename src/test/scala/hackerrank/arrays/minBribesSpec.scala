package hackerrank

import org.specs2.mutable.Specification

object minBribesSpec extends Specification {

  "calcBribes" >> {
    "should calc given 3 bribes" >> {
      val bribed = Map(0 -> true, 1 -> true, 3 -> true)
      val index = 2
      MinBrides.calcBribes(bribed, index) must beEqualTo(2)
    }

    "should report no brides when high index" >> {
      val bribed = Map(0 -> true, 1 -> true, 3 -> true)
      val index = 0
      MinBrides.calcBribes(bribed, index) must beEqualTo(0)
    }
  }

  "findThoseBribed" >> {
    "works for starting indexes" >> {
      val seen = Map[Int, Boolean]()
      val bribed = Map[Int, Boolean]()
      val index = 0
      val expected = Map[Int, Boolean]()
      MinBrides.findThoseBribed(seen, bribed, index) must beEqualTo(expected)
    }

    "should work for next indexes" >> {
      val seen = Map[Int, Boolean]()
      val bribed = Map[Int, Boolean]()
      val index = 1
      val expected = Map(0 -> true)
      MinBrides.findThoseBribed(seen, bribed, index) must beEqualTo(expected)
    }

    "should detect bribes for index 3" >> {
      val seen = Map[Int, Boolean]()
      val bribed = Map[Int, Boolean]()
      val index = 3
      val expected = Map(0 -> true, 1 -> true, 2 -> true)
      MinBrides.findThoseBribed(seen, bribed, index) must beEqualTo(expected)
    }

    "should detect no bribes when seen already" >> {
      val seen = Map(0 -> true, 1 -> true, 2 -> true)
      val bribed = Map[Int, Boolean]()
      val index = 3
      val expected = Map[Int, Boolean]()
      MinBrides.findThoseBribed(seen, bribed, index) must beEqualTo(expected)
    }
  }

  "minimumBribes" >> {
    "should work for no bribes" >> {
      val input = Array(1, 2, 3, 4, 5)
      MinBrides.minimumBribes(input) must beEqualTo(0)
    }

    "should work for single bribes for 5" >> {
      val input = Array(1, 2, 3, 5, 4)
      MinBrides.minimumBribes(input) must beEqualTo(1)
    }

    "should work for single bribes for 4" >> {
      val input = Array(1, 2, 4, 3, 5)
      MinBrides.minimumBribes(input) must beEqualTo(1)
    }

    "should work when bribed all the way to the back" >> {
      val input = Array(2, 3, 4, 5, 1)
      MinBrides.minimumBribes(input) must beEqualTo(4)
    }

    "should work when everyone bribes to the max" >> {
      val input = Array(3, 4, 5, 1, 2)
      MinBrides.minimumBribes(input) must beEqualTo(6)
    }

    "should work for internet example" >> {
      val input = Array(1, 2, 5, 3, 7, 8, 6, 4)
      MinBrides.minimumBribes(input) must beEqualTo(7)
    }

    "should fail when invalid bribe order happens" >> {
      val input = Array(5, 4, 3, 2, 1)
      MinBrides.minimumBribes(input) must beEqualTo("Too chaotic")
    }
  }
}