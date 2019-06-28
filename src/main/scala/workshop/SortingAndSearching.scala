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
    println(sortLogLines(4, logLines1))

    val logLines2 = List("[t2 13 121 98]", "[r1 box ape bit]", "[b4 xi me nu]", "[br8 eat nim did]", "[w1 has uni gry]", "[f3 52 54 31]")
    println(sortLogLines(6, logLines2))
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

  def sortLogLines(logFileSize: Int, logLines: List[String]): List[String] = {
    if(logLines.length == logFileSize) {
      qsortLogLines(logLines).map(line => line.substring(1, line.length - 1))
    }
    else logLines
  }

  // Problem 3: Write an algorithm to reorder the data in the log file,, according to defined rules
  def qsortLogLines(logLines: List[String]): List[String] = {

    if(logLines.length <= 1) logLines
    else {
      def isAllDigits(x: String) = x.matches("^\\d+$")

      def compareLogLine(s1: String, s2: String): Int = {
        val arr1 = s1.split(' ').tail // get s1 as array without identifier
        val arr2 = s2.split(' ').tail // get s2 as array without identifier

        // case when head 1 is string and head 2 is number
        if(!isAllDigits(arr1.head) && isAllDigits(arr2.head)) -1

        // case when head 1 is number and head 2 is string
        else if(isAllDigits(arr1.head) && !isAllDigits(arr2.head)) 1

        // case when both head 1 and head 2 is string or number
        else {
          if(arr1.mkString.compareTo(arr2.mkString) != 0) arr1.mkString.compareTo(arr2.mkString)
          else s1.compareTo(s2)
        }
      }

      val pivot = logLines(logLines.length / 2)
      List.concat(
        qsortLogLines(logLines filter(line => compareLogLine(line, pivot) < 0)),
        logLines filter(line => compareLogLine(line, pivot) == 0),
        qsortLogLines(logLines filter(line => compareLogLine(line, pivot) > 0))
      )
    }
  }
}
