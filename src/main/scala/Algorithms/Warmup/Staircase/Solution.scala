package Algorithms.Warmup.Staircase

import scala.io.StdIn

object Solution {
    def main(args: Array[String]): Unit = {
      val count = StdIn.readInt()

      1.to(count).foreach(l => println((" " * (count - l)) + ("#" * l)))
    }
}
