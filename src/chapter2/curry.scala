package chapter2

object curry extends App{
  def curry[A,B,C] (f: (A,B) => C): A => (B =>C) = x => (y => f(x,y))
  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = (x,y) => f(x)(y)
}