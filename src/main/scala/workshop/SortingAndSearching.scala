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
}
