package Algorithms.Warmup.PlusMinus

import scala.io.StdIn

object Solution {
  def main(args: Array[String]): Unit = {
    val count = StdIn.readInt()

    val reducer = (sums: (Int, Int, Int), number: Int) =>
      if (number < 0) (sums._1 + 1, sums._2, sums._3)
      else if (number == 0) (sums._1, sums._2 + 1, sums._3)
      else (sums._1, sums._2, sums._3 + 1)

    val counts = StdIn.readLine().split(' ').map(_.toInt).foldLeft((0, 0, 0))(reducer)

    val format = "%.6f"

    println(format.format(counts._3.toDouble / count))
    println(format.format(counts._1.toDouble / count))
    println(format.format(counts._2.toDouble / count))
  }
}
