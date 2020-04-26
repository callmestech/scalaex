package week1.recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def score(chars: List[Char], acc: Int = 0): Int =
      if (chars.isEmpty || acc < 0)
        acc
      else {
        val c = chars.head
        score(chars.tail, if (c == '(') acc + 1 else if (c == ')') acc - 1 else acc)
      }

    score(chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeCombinations(money: Int, coins: List[Int]): Int =
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
      // total combinations = using the first coin only + using all other coins
        countChangeCombinations(money - coins.head, coins) + countChangeCombinations(money, coins.tail)

    countChangeCombinations(money, coins)
  }
}
