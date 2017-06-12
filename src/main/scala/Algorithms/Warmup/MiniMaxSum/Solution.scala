package Algorithms.Warmup.MiniMaxSum

import scala.io.StdIn

object Solution {
  def main(args: Array[String]): Unit = {
    val numbers = StdIn.readLine().split(' ').map(_.toLong)

    val minSum = numbers.sum - numbers.max
    val maxSum = numbers.sum - numbers.min

    println(minSum.toString + ' ' + maxSum.toString)
  }
}
