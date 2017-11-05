package chapter2

object fibonacci extends App {
  
  def fib (n: Int): Int = {
    
    @annotation.tailrec
    def fibAcc (m: Int, acc1: Int, acc2: Int): Int = {
      if (m == n) acc1
      else fibAcc (m+1, acc2, acc1 + acc2)
    }
    
    fibAcc (1,0,1)
  }
  
  def formatResult (name: String, n: Int, f: Int => Int) = 
    s"The $name of $n is ${f(n)}"
  
  println (formatResult("Fibonacci", 10, fib))  
}