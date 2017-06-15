package WeekOfCode33.TransformToPalindrom

import org.scalatest.{FunSuite, Matchers}

class LongestPalindromicSubsequenceTest extends FunSuite with Matchers {
  test("Example 1") {
    val transMap = new TransMap()
      .add(1, 3)
      .add(5, 7)
      .add(3, 5)
      .add(2, 6)
      .add(8, 4)
      .add(10, 9)

    val input = List(1, 9, 2, 3, 10, 3)

    val result = LongestPalindromicSubsequence(transMap.reduce(input).toArray)

    result should be (5)
  }

  test("Example 2") {
    val transMap = new TransMap()
      .add(1, 2)
      .add(1, 3)
      .add(2, 7)
      .add(3, 1)
      .add(4, 5)
      .add(6, 8)
      .add(9, 6)
      .add(10, 5)

    val input = List(1, 4, 5, 7, 9, 8, 1, 3, 10, 4, 5, 10, 2, 7, 8)

    val result = LongestPalindromicSubsequence(transMap.reduce(input).toArray)

    result should be (10)
  }
}
