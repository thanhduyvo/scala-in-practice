package workshop

import scala.collection.mutable.Map

object ArraysAndStringsRunner {

  def main(args: Array[String]): Unit = {

    // Test
    println(ArraysAndStrings.findPermutations("abbc", "cbabadcbbabbcbabaabccbabc"))
  }

}

object ArraysAndStrings {

  // Problem 1.0: Find positions of all permutations of short string in long string
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

  // Problem 1.1: Is Unique: Implement an algorithm to determine if a string has all unique characters. What if you cannot use additional data structures?

  // Problem 1.2: Check Permutation: Given two strings,write a method to decide if one is a permutation of the other.

  // Problem 1.3: URLify: Write a method to replace all spaces in a string with '%20'.
  // You may assume that the string has sufficient space at the end to hold the additional characters,and that you are given the "true" length of the string.
  // (Note: If implementing in Java,please use a character array so that you can perform this operation in place.)
  // EXAMPLE
  // Input: "Mr John Smith ", 13
  // Output: "Mr%20John%20Smith"

  // Problem 1.4: Palindrome Permutation: Given a string, write a function to check if it is a permutation of a palin­ drome.
  // A palindrome is a word or phrase that is the same forwards and backwards. A permutation is a rearrangement of letters.
  // The palindrome does not need to be limited to just dictionary words.
  // EXAMPLE
  // Input: Tact Coa
  // Output: True (permutations: "taco cat", "atco eta", etc.)

  // Problem 1.5: One Away: There are three types of edits that can be performed on strings: insert a character, remove a character, or replace a character.
  // Given two strings, write a function to check if they are one edit (or zero edits) away.
  // EXAMPLE
  // pale, ple -> true
  // pales, pale -> true
  // pale, bale -> true
  // pale, bake -> false

  // Problem 1.6: String Compression: Implement a method to perform basic string compression using the counts of repeated characters.
  // For example, the string aabcccccaaa would become a2blc5a3.
  // If the "compressed" string would not become smaller than the original string, your method should return
  // the original string. You can assume the string has only uppercase and lowercase letters (a - z).

  // Problem 1.7: Rotate Matrix: Given an image represented by an NxN matrix, where each pixel in the image is 4 bytes,
  // write a method to rotate the image by 90 degrees. Can you do this in place?

  // Problem 1.8: Zero Matrix: Write an algorithm such that if an element in an MxN matrix is 0, its entire row and column are set to 0.

  // Problem 1.9: String Rotation:Assumeyou have a method isSubstringwhich checks if one word is a substring of another.
  // Given two strings, sl and s2, write code to check if s2 is a rotation of sl using only one call to isSubstring
  // (e.g.,"waterbottle" is a rotation of"erbottlewat").
}
