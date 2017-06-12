package Algorithms.Warmup.TimeConversion

import scala.io.StdIn

object Solution {
  def main(args: Array[String]): Unit = {
    val time = StdIn.readLine()
    val matches = """([\d]{2}):([\d]{2}):([\d]{2})(AM|PM)""".r

    val parts = time match {
      case matches("12", m, s, "AM") => ("00", m, s)
      case matches(h, m, s, "AM") => (h, m, s)
      case matches("12", m, s, "PM") => ("12", m, s)
      case matches(h, m, s, "PM") => ((h.toInt + 12).toString, m, s)
    }

    println(parts._1 + ":" + parts._2 + ":" + parts._3)
  }
}
