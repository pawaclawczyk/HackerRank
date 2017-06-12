package Algorithms.Warmup.DiagonalDifference

import scala.io.StdIn

object Solution {
  def main(args: Array[String]): Unit = {
    def abs(a: Int) : Int = if (a < 0) -a else a

    val lastRow = StdIn.readInt() - 1

    val diagonals = 0.to(lastRow).map(i => {
      val row = StdIn.readLine().split(' ').map(_.toInt)
      (row(i), row(lastRow - i))
    })

    val sums = diagonals.foldLeft((0, 0))((sum, cur) => (sum._1 + cur._1, sum._2 + cur._2))

    println(abs(sums._1 - sums._2))
  }
}
