package WeekOfCode33.TransformToPalindrom

import scala.collection.mutable
import scala.collection.Map
import scala.io.StdIn
import scala.math.max

object Solution {

  def main(args: Array[String]): Unit = {
    val params = StdIn.readLine().split(' ').map(_.toInt)

    val collector = new mutable.HashMap[Int, List[Int]]()

    for (i <- 1 to params(1)) {
      val transformation = StdIn.readLine().split(' ').map(_.toInt)

      val x = transformation(0)
      val y = transformation(1)

      val prevXs = if (collector.contains(x)) collector(x) else List()
      val prevYs = if (collector.contains(y)) collector(y) else List()

      collector += ((x, y :: prevXs ))
      collector += ((y, x :: prevYs ))
    }

    val transMap = collector.mapValues((l) => l.max)

    println(transformToHighest(1, transMap))
  }

  def transformToHighest(x: Int, transMap: Map[Int, Int]) : Int =
    if (transMap.contains(x)) transformToHighest(transMap(x), transMap)
    else x

  def lps(s: Array[Char], transMap: Map[Int, Int]): Int = {
    val visited = new mutable.HashMap[(Int, Int), Int]()

    def memo(k: (Int, Int), v: => Int): Int = {
      if (! (visited contains k)) visited += ((k, v))

      visited(k)
    }

    def iter(a: Int, b: Int): Int = {
      if (a == b) 1
      else if (b == a + 1 && transformToHighest(s(a), transMap) == transformToHighest(s(b), transMap)) 2
      else if (s(a) != s(b)) max(memo((a, b - 1), iter(a, b - 1)), memo((a + 1, b), iter(a + 1, b)))
      else memo((a + 1, b - 1), iter(a + 1, b - 1)) + 2
    }

    iter(0, s.length - 1)
  }
}


