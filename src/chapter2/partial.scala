package chapter2

object partial extends App{
  def partial1[A,B,C] (a: A, f: (A,B) => C): B => C = x => f(a,x)
  printf(partial1[Int,Int,Int] (1, (x,y) => x+y)(3).toString)
  
}