package chapter2

object sorted extends App{
  def isSorted[A] (as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def step(n: Int): Boolean = 
      if (n == as.length) true
      else if (!gt(as(n-1), as(n))) false
      else step(n+1)
    if (as.length<2) true else step(1)  
  }
  println(isSorted(Array(2,1,3), (x: Int,y: Int) => x<y))
  println(isSorted(Array(1,2,3), (x: Int,y: Int) => x<y))
  println(isSorted(Array(1,2,3,4,3,6), (x: Int,y: Int) => x<y))


}