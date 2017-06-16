package WeekOfCode33.PalindromicTable

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.io.StdIn

object Solution {
  val solver = new PalindromicDetector()

  def main(args: Array[String]): Unit = {
    val size = StdIn.readLine().split(' ').map(_.toInt)

    val table = 1.to(size(0)).map(i => StdIn.readLine().split(' ').map(_.toInt).toVector).toVector

    val solution = solver.search(table)

    println(solution.area)
    println(solution.topLeft.m.toString + " " + solution.topLeft.n + " " + solution.bottomRight.m + " " + solution.bottomRight.n)
  }
}

class Corner(val m: Int, val n: Int) {
  override def equals(o: scala.Any): Boolean =
    o match {
      case that: Corner => this.m == that.m && this.n == that.n
      case _ => false
    }

  override def toString: String =
    "(" + m + ", " + n + ")"
}

object Corner {
  def apply(coords: (Int, Int)) : Corner = coords match {
    case (m, n) => new Corner(m, n)
  }
}

class Rectangle(val topLeft: Corner, val bottomRight: Corner) {
  def height : Int = bottomRight.m - topLeft.m + 1
  def width : Int = bottomRight.n - topLeft.n + 1
  def area : Int = height * width

  def dropTopRow : Rectangle = new Rectangle(new Corner(topLeft.m + 1, topLeft.n), bottomRight)
  def dropBottomRow : Rectangle = new Rectangle(topLeft, new Corner(bottomRight.m - 1, bottomRight.n))
  def dropLeftColumn : Rectangle = new Rectangle(new Corner(topLeft.m, topLeft.n + 1), bottomRight)
  def dropRightColumn : Rectangle = new Rectangle(topLeft, new Corner(bottomRight.m, bottomRight.n - 1))

  def canSplitHorizontally : Boolean = height > 1
  def canSplitVertically : Boolean = width > 1

  def splitHorizontally : (Rectangle, Rectangle) = {
    if (!canSplitHorizontally) (this, this)
    else {
      val halfHeight = height / 2

      (
        new Rectangle(topLeft, new Corner(topLeft.m + halfHeight - 1, bottomRight.n)),
        new Rectangle(new Corner(topLeft.m + halfHeight, topLeft.n), bottomRight)
      )
    }
  }

  def splitVertically : (Rectangle, Rectangle) = {
    if (!canSplitVertically) (this, this)
    else {
      val halfWidth = width / 2
      (
        new Rectangle(topLeft, new Corner(bottomRight.m, topLeft.n + halfWidth - 1)),
        new Rectangle(new Corner(topLeft.m, topLeft.n + halfWidth), bottomRight)
      )
    }
  }

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case that : Rectangle => this.topLeft == that.topLeft && this.bottomRight == that.bottomRight
      case _ => false
    }

  override def toString: String =
    "[" + topLeft+ ", " + bottomRight + "]"
}

object Rectangle {
  def apply(topLeft: (Int, Int), bottomRight: (Int, Int)): Rectangle = new Rectangle(Corner(topLeft), Corner(bottomRight))
}

class PalindromicDetector {
  val statsMemory = new Memo[String, HashMap[Int, Int]]()
  val isPalindromicMemory = new Memo[String, Boolean]()

  def search(inTable: Vector[Vector[Int]]) : Rectangle = {
    val rectangle = Rectangle((0, 0), (inTable.size - 1, inTable.head.size - 1))

    def loopWithList(rs: List[Rectangle], ps: List[Rectangle]): Rectangle = {

      def generateSmaller(r: Rectangle) : List[Rectangle]
        = List(r.dropTopRow, r.dropBottomRow, r.dropLeftColumn, r.dropRightColumn)

      val (palindromic, notPalindromic) = rs.partition(PalindromicChecker.check(_, inTable))

      notPalindromic match {
        case Nil => {
          println(palindromic)
          palindromic.sortWith(_.area > _.area).head
        }
        case l => loopWithList(
          l.foldLeft(List[Rectangle]())((l, r) => l ::: generateSmaller(r)),
          palindromic
        )
      }
    }

    def loop(rectangle: Rectangle) : Rectangle = {
      if (isPalindromic(rectangle, inTable)) rectangle
      else {
        if (rectangle.width > rectangle.height) {
          val leftRect = if (rectangle.width > 1) loop(rectangle.dropRightColumn) else Rectangle((0, 0), (0, 0))
          val rightRect = if (rectangle.width > 1) loop(rectangle.dropLeftColumn) else Rectangle((0, 0), (0, 0))
          val topRect = if (rectangle.height > 1) loop(rectangle.dropBottomRow) else Rectangle((0, 0), (0, 0))
          val bottomRect = if (rectangle.height > 1) loop(rectangle.dropTopRow) else Rectangle((0, 0), (0, 0))

          max(topRect, bottomRect, leftRect, rightRect)
        } else {
          val topRect = if (rectangle.height > 1) loop(rectangle.dropBottomRow) else Rectangle((0, 0), (0, 0))
          val bottomRect = if (rectangle.height > 1) loop(rectangle.dropTopRow) else Rectangle((0, 0), (0, 0))
          val leftRect = if (rectangle.width > 1) loop(rectangle.dropRightColumn) else Rectangle((0, 0), (0, 0))
          val rightRect = if (rectangle.width > 1) loop(rectangle.dropLeftColumn) else Rectangle((0, 0), (0, 0))

          max(topRect, bottomRect, leftRect, rightRect)
        }
      }
    }

//    loop(rectangle)

    loopWithList(List(rectangle), List())
  }

  def max(rectangles: Rectangle*) : Rectangle = {
    rectangles.reduceLeft((p, c) => if (p.area > c.area) p else c)
  }

  def isPalindromic(rectangle: Rectangle, inTable: Vector[Vector[Int]]) : Boolean = {
    if (isPalindromicMemory.contains(rectangle.toString)) isPalindromicMemory.get(rectangle.toString)
    else {
      val stats = collectStats(rectangle, inTable)
      val odd = stats.filter{ case (k, v) => v % 2 == 1}
      val result = odd.size < 2

      isPalindromicMemory.set(rectangle.toString, result)

      result
    }

  }

  def collectStats(rectangle: Rectangle, inTable: Vector[Vector[Int]]) : HashMap[Int, Int] = {
    if (statsMemory.contains(rectangle.toString)) {
      statsMemory.get(rectangle.toString)
    } else {
      val stats = if (rectangle.topLeft == rectangle.bottomRight)
        HashMap(inTable(rectangle.topLeft.m)(rectangle.topLeft.n) -> 1)
      else if (rectangle.width > rectangle.height)
        rectangle.splitVertically match {
          case (left, right) => joinStats(collectStats(left, inTable), collectStats(right, inTable))
        }
      else
        rectangle.splitHorizontally match {
          case (top, bottom) => joinStats(collectStats(top, inTable), collectStats(bottom, inTable))
        }

      statsMemory.set(rectangle.toString, stats)

      stats
    }
  }

  def joinStats(a: HashMap[Int, Int], b: HashMap[Int, Int]) : HashMap[Int, Int] = {
    def sum(keyInA: Int, value: Int) : Int =
      if (a.contains(keyInA)) a(keyInA) + value else value

    b.foldLeft(a) { case (joint, (k, v)) => joint + (k -> sum(k, v)) }
  }
}

class Memo[K, V] {
  val memory = new mutable.HashMap[K, V]()

  def contains(k: K) : Boolean = memory.contains(k)

  def get(k: K) : V = memory(k)

  def set(k: K, v: V) : Unit = memory += (k -> v)
}

object PalindromicChecker {
  def check(r: Rectangle, t: Vector[Vector[Int]]): Boolean = {
    val stats = new mutable.HashMap[Int, Int]()

    for {
      i <- r.topLeft.m to r.bottomRight.m
      j <- r.topLeft.n to r.bottomRight.n
    } {
      val v = t(i)(j)

      stats += (v -> (stats.getOrElse(v, 0) + 1))
    }

    val odd = stats.filter{
      case (k, v) => v % 2 == 1
    }

    odd.size < 2
  }
}