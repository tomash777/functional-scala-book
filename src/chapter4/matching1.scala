package chapter4

object matching1 {
  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =  
    try { Some(Pattern.compile(s)) } catch {case e: PatternSyntaxException => None}
    
  def mkMatcher(pat: String): Option[String => Boolean] = 
    pattern(pat) map (p => s => p.matcher(s).matches)
  
  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {p <- pattern(pat)} yield ((s: String) => p.matcher(s).matches) 
      
  def doesMatch(pat: String, s:String): Option[Boolean] =
    for {p <- mkMatcher_1(pat)} yield p(s)
    
  def bothMatch(pat: String, pat1: String, s: String): Option[Boolean] =
    for {p1 <- mkMatcher_1(pat); p2 <- mkMatcher_1(pat1)} yield p1(s) && p2(s)
      
  def bothMatch_1(pat: String, pat1: String, s: String): Option[Boolean] = 
    mkMatcher(pat).flatMap(f => (mkMatcher(pat1) map (g => f(s) && g(s))))
    
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap (x => (b map (y => f(x,y))))
  
  def bothMatch_2(pat: String, pat1: String, s: String): Option[Boolean] = {
      map2(mkMatcher(pat), mkMatcher(pat1))((f,g) => f(s) && g(s))
    }
  
 
    
    
}