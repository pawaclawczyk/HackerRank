package WeekOfCode33.TransformToPalindrom

import org.scalatest.{FunSuite, Matchers}

class TransMapTest extends FunSuite with Matchers {
  test("Empty returns value without transformations") {
    val tm = new TransMap()

    tm(42) should be (42)
  }

  test("Returns minimal value defined by transformation") {
    val tm = new TransMap()
      .add(3, 5)

    tm(3) should be (3)
    tm(5) should be (3)
  }

  test("Bidirectional transformations") {
    val tm = new TransMap()
      .add(3, 5)
      .add(5, 3)

    tm(3) should be (3)
    tm(5) should be (3)
  }

  test("Circular transformations") {
    val tm = new TransMap()
      .add(3, 5)
      .add(3, 8)
      .add(8, 5)

    tm(3) should be (3)
    tm(5) should be (3)
    tm(8) should be (3)
  }

  test("Class with 1000 elements") {
    val empty = new TransMap()

    val tm = 1.to(1000).foldLeft(empty)((tm, i) => if (i == 1000) tm.add(1000, 1) else tm.add(i, i + 1))

    tm(789) should be (1)
    tm(1001) should be (1001)
  }

  test("reduce input") {
    val input = List(1, 9, 2, 3, 10, 3)
    val transMap = new TransMap()
      .add(1, 3)
      .add(5, 7)
      .add(3, 5)
      .add(2, 6)
      .add(8, 4)
      .add(10, 9)

    transMap.reduce(input) should be (List(1, 9, 2, 1, 9, 1))
  }
}
