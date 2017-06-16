package WeekOfCode33.PalindromicTable

import org.scalatest.{FunSuite, Matchers}

class PalindromicDetectorTest extends FunSuite with Matchers {
//  test("Example 1") {
//    val table = Vector(
//      Vector(1, 1, 1),
//      Vector(1, 0, 1),
//      Vector(1, 1, 1)
//    )
//
//    val detector = new PalindromicDetector()
//
//    detector.search(table) should be (9)
//  }

  test("Example 2") {
    val table = Vector(
      Vector(1, 2, 0, 3, 2),
      Vector(0, 1, 2, 3, 4),
      Vector(0, 9, 8, 9, 0)
    )

    val detector = new PalindromicDetector()

    detector.search(table) should be (Rectangle((0, 0), (1, 3)))
    detector.search(table).area should be (8)
  }

//  test("Example 2. Collect stats") {
//    val table = Vector(
//      Vector(1, 2, 0, 3, 2),
//      Vector(0, 1, 2, 3, 4),
//      Vector(0, 9, 8, 9, 0)
//    )
//
//    val rectangle = Rectangle((0, 0), (2, 4))
//
//    val detector = new PalindromicDetector()
//
//    val stats = detector.collectStats(rectangle, table)
//
//    println(stats)
//  }
}
