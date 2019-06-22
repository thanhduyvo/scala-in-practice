package workshop

import scala.math.Ordering

object SortingAndSearchingRunner {

  def main(args: Array[String]): Unit = {

    // Test
    println(SortingAndSearching.msort(List(8, 6, 4, 9, 1, 2)))
  }

}

object SortingAndSearching {

  // Merge Sort
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

  val fruits = List("apple", "pineapple", "orange", "banana")
  val sortedList = msort(fruits)
  println(sortedList)
}
