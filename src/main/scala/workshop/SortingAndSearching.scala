package workshop

import workshop.SortingAndSearching._
import scala.math.Ordering

object SortingAndSearchingRunner {

  def main(args: Array[String]): Unit = {

    val fruits = List("apple", "pineapple", "orange", "banana")
    val numbers = List(8, 6, 4, 9, 1, 2)

    // Test
    println(msort(numbers))
    println(msort(fruits))

    println(qsort(numbers))

    val logLines1 = List("[mi2 jog mid pet]", "[wz3 34 54 398]", "[a1 alps cow bar]", "[x4 45 21 7]")
    val logLines2 = List("[t2 13 121 98]", "[r1 box ape bit]", "[b4 xi me nu]", "[br8 eat nim did]", "[w1 has uni gry]", "[f3 52 54 31]")
    println(sortLogLines(logLines1))
  }

}

object SortingAndSearching {

  // Problem 1: Implement Merge Sort algorithm
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if(n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if(ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (ls, rs) = xs.splitAt(n)
      merge(msort(ls), msort(rs))
    }
  }

  // Problem 2: Implement Quick Sort algorithm
  def qsort(xs: List[Int]): List[Int] = {
    if(xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      List.concat(
        qsort(xs filter (pivot >)),
        xs filter (pivot ==),
        qsort(xs filter (pivot <))
      )
    }
  }

  // Problem 3: Write an algorithm to reorder the data in the log file,, according to defined rules
  def sortLogLines(logLines: List[String]): List[String] = {
    if(logLines.length <= 1) logLines
    else {
      def isAllDigits(x: String) = x.matches("^\\d+$")

      def compareLogLine(s1: String, s2: String): Int = {
        val split1 = s1.split(' ')(1)
        val split2 = s2.split(' ')(1)
        if(!isAllDigits(split1) && isAllDigits(split2)) 1
        else if(isAllDigits(split1) && !isAllDigits(split2)) -1
        else {
          val comp1 = s1.split(' ').tail.toString
          val comp2 = s2.split(' ').tail.toString
          if(comp1.compareTo(comp2) != 0) comp1.compareTo(comp2)
          else s1.compareTo(s2)
        }
      }

      val pivot = logLines(logLines.length / 2)
      List.concat(
        sortLogLines(logLines filter(line => compareLogLine(line, pivot) < 0)),
        logLines filter(line => compareLogLine(line, pivot) == 0),
        sortLogLines(logLines filter(line => compareLogLine(line, pivot) > 0))
      )
    }
  }
}
