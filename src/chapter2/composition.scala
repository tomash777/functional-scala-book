package chapter2

object composition extends App{
  def compose[A,B,C] (f: B => C, g: A => B): A => C = x => f(g(x)) 
  def compose1[A,B,C] (f: B => C, g: A => B): A => C = x => (g andThen f)(x)
  printf(compose[Int,Int,Int](x => x * x, x => x + 2)(2).toString)

}