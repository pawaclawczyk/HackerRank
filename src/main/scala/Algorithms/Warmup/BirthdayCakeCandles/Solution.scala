package Algorithms.Warmup.BirthdayCakeCandles

import scala.io.StdIn

object Solution {
  def main(args: Array[String]): Unit = {
    val count = StdIn.readInt()
    val candles = StdIn.readLine().split(' ').map(_.toInt)
    val highestCandle = candles.max

    val numberOfHighestCandles = candles.foldLeft(0)((count, height) => if (height == highestCandle) count + 1 else count)

    println(numberOfHighestCandles)
  }
}
