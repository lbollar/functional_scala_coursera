package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */

  // pascal(0,2)
  // 1+pascal(0,1)
  // 1+1+pascal(-1,0)
  def pascal(c: Int, r: Int): Int = {
    if (c==0 || r==c) 1
      else pascal(c,r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {
    def balanceWithSum(chars: List[Char], sum: Int): Boolean = {
      if (sum < 0) false
      else if (chars.isEmpty && sum == 0) true
      else if (chars.isEmpty && sum != 0) false
      else if (chars.head == '(') balanceWithSum(chars.tail, sum + 1)
      else if (chars.head == ')') balanceWithSum(chars.tail, sum - 1)
      else balanceWithSum(chars.tail,sum)
    }
    balanceWithSum(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

}
