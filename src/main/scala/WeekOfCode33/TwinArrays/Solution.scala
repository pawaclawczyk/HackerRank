package WeekOfCode33.TwinArrays

import scala.io.StdIn

object Solution {
  def main(args: Array[String]): Unit = {
    val size = StdIn.readInt()
    val A = StdIn.readLine().split(' ').map(x => x.toInt).zipWithIndex.sortBy(_._1)
    val B = StdIn.readLine().split(' ').map(x => x.toInt).zipWithIndex.sortBy(_._1)

    def minFromB(indexOfA: Int): Int = {
      if (B(0)._2 == indexOfA) B(1)._1 else B(0)._1
    }

    val minSum = A(0)._1 + minFromB(A(0)._2)

    def minIter(indexOfA: Int, minSum: Int): Int = {
      val a = A(indexOfA)
      val sum = a._1 + minFromB(a._2)

      if (sum > minSum) minSum else minIter(indexOfA + 1, sum)
    }

    println(minIter(1, minSum))
  }
}
