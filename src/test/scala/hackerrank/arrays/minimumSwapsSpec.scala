package hackerrank

import org.specs2.mutable.Specification



object MinimumSwapsSpec extends Specification {

  // decideOnSwap(toSort: Array[Int], currentItem: Map[Int, Int], needsSwapping: Map[Int, Int]): Array[Int] = {
  "swap" >> {
    "should be able to do a simple swap" >> {
      val toSort = Array(2, 1, 3)
      val currentItem = (2, 0) // value 2 is at position 0
      val expected = Array(1, 2, 3)
      MinimumSwaps.swapItems(toSort, currentItem) must beEqualTo(expected)
    }

    "should be able to do a more complex swap" >> {
      val toSort = Array(5, 2, 3, 4, 1)
      val currentItem = (5, 0) // value 5 is at position 0
      val expected = Array(1, 2, 3, 4, 5)
      MinimumSwaps.swapItems(toSort, currentItem) must beEqualTo(expected)
    }
  }

  "minimumSwaps" >> {
    "should do 0 swaps for a sorted list" >> {
      val input = Array(1, 2, 3)
      MinimumSwaps.minimumSwaps(input) must beEqualTo(0)
    }

    "should do 1 swaps for a simple list" >> {
      val input = Array(2, 1, 3)
      MinimumSwaps.minimumSwaps(input) must beEqualTo(1)
    }

    "should do 5 swaps on a more complex array" >> {
      val input = Array(7, 1, 3, 2, 4, 5, 6)
      MinimumSwaps.minimumSwaps(input) must beEqualTo(5)
    }
  }
}
