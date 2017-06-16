package WeekOfCode33.PalindromicTable

import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.HashMap
import scala.util.Random

class RectangleTest extends FunSuite with Matchers {
  /**
    * (0, 0) (0, 1) (0, 2) (0, 3)
    * (1, 0) (1, 1) (1, 2) (1, 3)
    * (2, 0) (2, 1) (2, 2) (2, 3)
    * (3, 0) (3, 1) (3, 2) (3, 3)
    */
  val evenSquare = Rectangle((0, 0), (3, 3))
  val evenSquareTop = Rectangle((0, 0), (1, 3))
  val evenSquareBottom = Rectangle((2, 0), (3, 3))
  val evenSquareLeft = Rectangle((0, 0), (3, 1))
  val evenSquareRight = Rectangle((0, 2), (3, 3))

  /**
    * (0, 0) (0, 1) (0, 2) (0, 3) (0, 4)
    * (1, 0) (1, 1) (1, 2) (1, 3) (1, 4)
    * (2, 0) (2, 1) (2, 2) (2, 3) (2, 4)
    * (3, 0) (3, 1) (3, 2) (3, 3) (3, 4)
    * (4, 0) (4, 1) (4, 2) (4, 3) (4, 4)
    */
  val oddSquare = Rectangle((0, 0), (4, 4))
  val oddSquareTop = Rectangle((0, 0), (1, 4))
  val oddSquareBottom = Rectangle((2, 0), (4, 4))
  val oddSquareLeft = Rectangle((0, 0), (4, 1))
  val oddSquareRight = Rectangle((0, 2), (4, 4))

  /**
    * (0, 0) (0, 1) (0, 2) (0, 3) (0, 4) (0, 5)
    * (1, 0) (1, 1) (1, 2) (1, 3) (1, 4) (1, 5)
    * (2, 0) (2, 1) (2, 2) (2, 3) (2, 4) (2, 5)
    * (3, 0) (3, 1) (3, 2) (3, 3) (3, 4) (3, 5)
    */
  val rectangle = Rectangle((0, 0), (3, 5))
  val rectangleTop = Rectangle((0, 0), (1, 5))
  val rectangleBottom = Rectangle((2, 0), (3, 5))
  val rectangleLeft = Rectangle((0, 0), (3, 2))
  val rectangleRight = Rectangle((0, 3), (3, 5))

  /**
    * (0, 0)
    * (1, 0)
    * (2, 0)
    * (3, 0)
    * (4, 0)
    */
  val column = Rectangle((0, 0), (4, 0))
  val columnTop = Rectangle((0, 0), (1, 0))
  val columnBottom = Rectangle((2, 0), (4, 0))

  /**
    * (0, 0) (0, 1) (0, 2) (0, 3) (0, 4)
    */
  val row = Rectangle((0, 0), (0, 4))
  val rowLeft = Rectangle((0, 0), (0, 1))
  val rowRight = Rectangle((0, 2), (0, 4))

  val cell = Rectangle((42, 42), (42, 42))

  test("split even square horizontally") {
    val (topRectangle, bottomRectangle) = evenSquare.splitHorizontally

    topRectangle should be (evenSquareTop)
    bottomRectangle should be (evenSquareBottom)
  }

  test("split odd square horizontally") {
    val (topRectangle, bottomRectangle) = oddSquare.splitHorizontally

    topRectangle should be (oddSquareTop)
    bottomRectangle should be (oddSquareBottom)
  }

  test("split rectangle horizontally") {
    val (topRectangle, bottomRectangle) = rectangle.splitHorizontally

    topRectangle should be (rectangleTop)
    bottomRectangle should be (rectangleBottom)
  }

  test("split column horizontally") {
    val (topRectangle, bottomRectangle) = column.splitHorizontally

    topRectangle should be (columnTop)
    bottomRectangle should be (columnBottom)
  }

  test("split row horizontally") {
    val (topRectangle, bottomRectangle) = row.splitHorizontally

    topRectangle should be (row)
    bottomRectangle should be (row)
  }

  test("split cell horizontally") {
    val (topRectangle, bottomRectangle) = cell.splitHorizontally

    topRectangle should be (cell)
    bottomRectangle should be (cell)
  }

  test("split even square vertically") {
    val (leftRectangle, rightRectangle) = evenSquare.splitVertically

    leftRectangle should be (evenSquareLeft)
    rightRectangle should be (evenSquareRight)
  }

  test("split odd square vertically") {
    val (leftRectangle, rightRectangle) = oddSquare.splitVertically

    leftRectangle should be (oddSquareLeft)
    rightRectangle should be (oddSquareRight)
  }

  test("split rectangle vertically") {
    val (leftRectangle, rightRectangle) = rectangle.splitVertically

    leftRectangle should be (rectangleLeft)
    rightRectangle should be (rectangleRight)
  }

  test("split column vertically") {
    val (leftRectangle, rightRectangle) = column.splitVertically

    leftRectangle should be (column)
    rightRectangle should be (column)
  }

  test("split row vertically") {
    val (leftRectangle, rightRectangle) = row.splitVertically

    leftRectangle should be (rowLeft)
    rightRectangle should be (rowRight)
  }

  test("split cell vertically") {
    val (leftRectangle, rightRectangle) = cell.splitVertically

    leftRectangle should be (cell)
    rightRectangle should be (cell)
  }

//  test("collect stats from table") {
//    val rectangle = Rectangle((0, 0), (2, 2))
//    val table = Vector(
//      Vector(1, 2, 3),
//      Vector(2, 3, 1),
//      Vector(3, 1, 2)
//    )
//
//    rectangle.collectStats(table) should be (HashMap(1 -> 3, 2 -> 3, 3 -> 3))
//  }
//
//  test("collect stats from rectangular in table") {
//    val rectangle = Rectangle((1, 1), (2, 2))
//    val table = Vector(
//      Vector(1, 2, 3),
//      Vector(2, 3, 1),
//      Vector(3, 1, 2)
//    )
//
//    rectangle.collectStats(table) should be (HashMap(1 -> 2, 2 -> 1, 3 -> 1))
//  }
//
//  test("collect stats from big table") {
//    var random = Random
//    val table = 1.to(320).map(i => 1.to(320).map(i => random.nextInt(10)).toVector).toVector
//    val rectangle = Rectangle((0, 0), (319, 319))
//
//    val stats = rectangle.collectStats(table)
//
//    println(stats)
//  }
}
