package workshop

object MiscRunner {

  def main(args: Array[String]): Unit = {

    // Test

    println(Misc.product(x => x * x)(3, 4))

    println(Misc.fact(5))

    println(Misc.removeAt(List('a', 'b', 'c', 'd'), 0))
  }

}

object Misc {

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def fact(n: Int) = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if(a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  def removeAt[T](xs: List[T], n: Int): List[T] = {
    if(n > xs.length - 1) xs
    else {
      xs match {
        case List() => List()
        case y :: ys => if(y == xs(n)) ys else List(y) ++ removeAt(ys, n - 1)
      }
    }
  }

}
