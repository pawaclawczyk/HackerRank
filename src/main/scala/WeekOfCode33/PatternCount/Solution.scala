package WeekOfCode33.PatternCount

import scala.io.StdIn
import scala.util.matching.Regex

object Solution {
  def main(args: Array[String]): Unit = {
    val queries = StdIn.readInt()

    for (i <- 1 to queries) {
      println(PatternCount.count(StdIn.readLine(), 0))
    }
  }
}

object PatternCount {
  val pattern: Regex = """1([0]+)1""".r

  def count(line: String, alreadyMatched: Int): Int = {
    pattern.findFirstMatchIn(line) match {
      case None => alreadyMatched
      case Some(m) => count("1" + m.after.toString, alreadyMatched + 1)
    }
  }
}
