package chapter4

object eithers extends App{
  trait Either[+E, +A]{
  case class Left[+E] (value: E) extends Either[E, Nothing]
  case class Right[+A] (value: A) extends Either[Nothing, A]
  
  def map[B] (f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  
  def flatMap[EE >:E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  
  def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }
  
  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    flatMap (a => (b map (x => f(a,x))))
  
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum/xs.length)
    
  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {Right(x/y)} catch {case e: Exception => Left(e)}
  
  
  
  
  }
}