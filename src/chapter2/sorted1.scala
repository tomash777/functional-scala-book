package chapter2

object sorted1 extends App{
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def go(n: Int): Boolean = n >= as.length || (gt(as(n-1),as(n)) && go(n+1))
    go(1)

  }
   println(isSorted(Array(2,1,3), (x: Int,y: Int) => x<y))
  println(isSorted(Array(1,2,3), (x: Int,y: Int) => x<y))
  println(isSorted(Array(1,2,3,4,3,6), (x: Int,y: Int) => x<y))
}