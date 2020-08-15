package hackerrank.hashmaps

import org.specs2.mutable.Specification

class SherlockAndAnagramsSpec extends Specification {
  "calcAnagrams" >> {
    "should calc anagrams on for size 1" >> {
      val input = Array('a', 'a', 'a')
      SherlockAndAnagrams.calcAnagrams(1, input) must beEqualTo(3)
    }

    "should calc anagrams on for size 2" >> {
      val input = Array('a', 'a', 'a')
      SherlockAndAnagrams.calcAnagrams(2, input) must beEqualTo(1)
    }

    "should calc anagrams on for size 1 for cdcd" >> {
      val input = Array('c', 'd', 'c', 'd')
      SherlockAndAnagrams.calcAnagrams(1, input) must beEqualTo(2)
    }

    "should calc anagrams on for size 2 for cdcd" >> {
      val input = Array('c', 'd', 'c', 'd')
      SherlockAndAnagrams.calcAnagrams(2, input) must beEqualTo(3)
    }

    "should calc anagrams on for size 3 for cdcd" >> {
      val input = Array('c', 'd', 'c', 'd')
      SherlockAndAnagrams.calcAnagrams(3, input) must beEqualTo(0)
    }
  }
  "sherlockAndAnagrams" >> {
    "should calc simple negative example" >> {
      val input = "abcd"
      SherlockAndAnagrams.sherlockAndAnagrams(input) must beEqualTo(0)
    }

    "should calc simple example" >> {
      val input = "abba"
      SherlockAndAnagrams.sherlockAndAnagrams(input) must beEqualTo(4)
    }

    "should calc cdcd example" >> {
      val input = "cdcd"
      SherlockAndAnagrams.sherlockAndAnagrams(input) must beEqualTo(5)
    }

    "should calc cdcd example" >> {
      val input = "ifailuhkqq"
      SherlockAndAnagrams.sherlockAndAnagrams(input) must beEqualTo(3)
    }

    "should calc internet example" >> {
      val input = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      SherlockAndAnagrams.sherlockAndAnagrams(input) must beEqualTo(166650)
    }
  }
}
