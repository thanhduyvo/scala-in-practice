package workshop

object RecursionRunner {

  def main(args: Array[String]): Unit = {

    // Test
  }

}

object Recursion {

  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }

}
