package chapter2

object fibonacci1 extends App{
  
  def factorial(n:Int): Int = {
    def factAcc(m: Int, acc: Int): Int = {
      if (m==0) acc
      else factAcc(m-1,m*acc)
    }
    factAcc(n,1)
  }
 println ("factorial of 10 is "+ factorial
     (10)) 
  
  def fib(n: Int): Int = {
   def fibAcc(m: Int, a: Int, b: Int): Int ={
     if (m == n) a
     else fibAcc(m+1, b, a+b)
   }
   fibAcc(1,0,1)
 }
     
     
}