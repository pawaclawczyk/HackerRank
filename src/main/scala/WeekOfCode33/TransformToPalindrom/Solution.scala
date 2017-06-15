package WeekOfCode33.TransformToPalindrom

import scala.collection.mutable
import scala.io.StdIn
import scala.math.max
import scala.collection.immutable.TreeSet

object Solution {
  def main(args: Array[String]): Unit = {
    val params = StdIn.readLine().split(' ').map(_.toInt)
    val numberOfTransformations = params(1)

    val transMap = 1.to(numberOfTransformations)
      .foldLeft(new TransMap())((tm, i) => {
        val t = StdIn.readLine().split(' ').map(_.toInt)
        tm.add(t(0), t(1))
      })

    val input = StdIn.readLine().split(' ').map(_.toInt)

    val reducedInput = transMap.reduce(input).toArray

    println(LongestPalindromicSubsequence(reducedInput))
  }
}

object LongestPalindromicSubsequence {
  val memo = new Memo[(Int, Int), Int]

  def apply(sequence: Array[Int]): Int = {
    def loop(a: Int, b: Int): Int = {
      if (a == b) 1
      else if (b == a + 1 && sequence(a) == sequence(b) ) 2
      else if (sequence(a) != sequence(b)) max(
        memo.get((a, b - 1), loop(a, b - 1)),
        memo.get((a + 1, b), loop(a + 1, b))
      )
      else memo.get((a + 1, b - 1), loop(a + 1, b - 1)) + 2
    }

    loop(0, sequence.length - 1)
  }
}

class TransMap(val classes: List[TreeSet[Int]], val memo: Memo[Int, Int]) {
  def this() = this(List(), new Memo[Int, Int]())

  def add(x: Int, y: Int) : TransMap = {
    val subclasses = classes.groupBy(s => if (s.contains(x)) 'X' else if (s.contains(y)) 'Y' else 'R')

    val classOfX = if (subclasses.contains('X')) subclasses('X').head else TreeSet.empty[Int] + x
    val classOfY = if (subclasses.contains('Y')) subclasses('Y').head else TreeSet.empty[Int] + y
    val classesOfR = if (subclasses.contains('R')) subclasses('R') else Nil

    new TransMap((classOfX union classOfY) :: classesOfR, memo)
  }

  def reduce(input: Iterable[Int]) : Iterable[Int] = input.map(this(_))

  def apply(x: Int) : Int = {
    val containing = classes.filter(s => s contains x)

    val result = containing match {
      case Nil => x
      case s :: ss => s.min
    }

    memo.get(x, result)
  }
}

class Memo[K, V] {
  val memory = new mutable.HashMap[K, V]()

  def get(k: K, v: => V): V = {
    if (! memory.contains(k)) memory += ((k, v))

    memory(k)
  }
}
