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
  def pascal(c: Int, r: Int): Int = {
    def factorial(n: BigInt): BigInt = {
      def fact(n: BigInt, acc: BigInt): BigInt =
        if (n == 0) acc else fact(n - 1, n * acc)

      fact(n, 1)
    }

    (factorial(BigInt(r)) / (factorial(BigInt(c)) * factorial(BigInt(r - c)))).intValue()
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(chars: List[Char], parSum: Int): Boolean =
      if (chars.isEmpty) (parSum == 0)
      else if (parSum<0) false
      else if (chars.head == '(')
        check(chars.tail, parSum + 1)
      else if (chars.head == ')') check(chars.tail, parSum - 1)
      else check(chars.tail, parSum)
      
    check(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int]): Int =
      if(money==0) 1
      else if (money<0 || coins.isEmpty) 0
      else loop(money-coins.head, coins) + loop(money,coins.tail)
      
    if(money<=0) 0
    else loop(money,coins)
  }
}
