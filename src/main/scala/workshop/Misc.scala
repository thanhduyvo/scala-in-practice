package workshop

import workshop.Misc._

object MiscRunner {

  def main(args: Array[String]): Unit = {

    // Test

    println(product(x => x * x)(3, 4))

    println(fact(5))

    println(removeAt(List('a', 'b', 'c', 'd'), 0))

    println(show(Sum(Number(1), Number(44))))

    println(show(Sum(Prod(Number(2), Var("x")), Var("y"))))

    println(show(Prod(Sum(Number(2), Var("x")), Var("y"))))

    println(pack(List("a", "a", "a", "b", "b", "b", "b", "b", "c", "c", "a")))

    println(encode(List("a", "a", "a", "b", "c", "c", "a")))
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

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Var(x: String) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
    case Var(x) => x
    case Prod(l, r) => (if(l.isInstanceOf[Sum]) "(" else "") + show(l) + (if(l.isInstanceOf[Sum]) ")" else "") + " * " + (if(r.isInstanceOf[Sum]) "(" else "") + show(r) + (if(r.isInstanceOf[Sum]) ")" else "")
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if(x <= y) x :: y :: ys else y :: insert(x, ys)
  }

  def mapSeq[A, B](lst: List[A], f: A => B): List[B] = lst match {
    case Nil => Nil
    case h :: t => f(h) :: mapSeq(t, f)
  }

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => (x :: xs1.takeWhile(x1 => x1 == x)) :: pack(xs1.dropWhile(x1 => x1 == x))
  }

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case _ => pack(xs).map(x => (x.head, x.length))
  }
}
