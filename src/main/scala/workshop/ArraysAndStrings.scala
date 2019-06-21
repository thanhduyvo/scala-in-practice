package workshop

import scala.collection.mutable.Map

object ArraysAndStringsRunner {

  def main(args: Array[String]): Unit = {

    // Test
    println(ArraysAndStrings.findPermutations("abbc", "cbabadcbbabbcbabaabccbabc"))
  }

}

object ArraysAndStrings {

  def findPermutations(short: String, long: String): List[Int] = {
    val ms = Map[Char, Int]()
    for(c <- short) {
      if(!ms.contains(c)) ms += (c -> 1)
      else ms(c) += 1
    }

    var lst = List[Int]()
    for(i <- 0 to long.length - short.length) {
      val s = long.slice(i, i + short.length)
      if(isPermutation(s, Map(ms.toSeq:_*))) lst = i :: lst
    }
    lst
  }

  def isPermutation(s: String, ms: Map[Char, Int]): Boolean = {
    for(c <- s) if(ms.contains(c)) ms(c) -= 1
    !ms.values.exists(v => v != 0)
  }
}
