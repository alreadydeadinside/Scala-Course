package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("pascal: ")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("balance: " + balance("(if (zero? x) max (/ 1 x))".toList))
    println("countChange: " + countChange(10, List(1, 2, 3, 4, 5)))
    println("variant 18 function: " + foo(2, 5))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || (c == r)) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def isBalanced(chars: List[Char], balance: Int): Boolean = {
      if (chars.isEmpty) {
        if (balance == 0) true
        else false
      }
      else if (balance < 0) false
      else if (chars.head == '(') isBalanced(chars.tail, balance + 1)
      else if (chars.head == ')') isBalanced(chars.tail, balance - 1)
      else isBalanced(chars.tail, balance)
    }

    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 || coins.isEmpty) 0
    else if (money == coins.head) 1 + countChange(money, coins.tail)
    else if (money < coins.head) countChange(money, coins.tail)
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  /**
   * Exercise 4
   * Variant 18
   */
  def foo(k: Int, x: Int): Int = {
    if (x > 1 && x < k) k
    else {
      require(x > k && x >= 0, "Invalid values")
      fact(x)
    }
  }

  def fact(x: Int): Int = {
    if (x <= 0) 1
    else x * fact(x - 1)
  }
}
