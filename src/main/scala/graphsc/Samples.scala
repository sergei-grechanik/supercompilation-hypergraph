package graphsc

object Samples {
  
  private val defs = collection.mutable.Map[String, String]()
  
  private def p(s: String) {
    defs += s.takeWhile(_ != ' ') -> s
  }
  
  p("add x y = case x of { Z -> y; S x -> S (add x y) }")
  p("mul x y = case x of { Z -> Z; S x -> add y (mul x y) }")
  p("padd x y = case x of { Z -> y; S x -> S (padd y x) }")
  p("pmul x y = case x of { Z -> Z; S x -> padd y (pmul y x) }")
  p("id x = case x of {Z -> Z; S x -> S (id x)}")
  p("nrev x = case x of {Z -> Z; S x -> add (nrev x) (S Z)}")
  p("fac x = case x of {Z -> S Z; S x -> mul (S x) (fac x)}")
  p("fib x = case x of {Z -> Z; S x -> case x of {Z -> S Z; S x -> add (fib (S x)) (fib x)}}")
  p("append x y = case x of {N -> y; C a x -> C a (append x y)}")
  p("nrevL x = case x of {N -> N; C a x -> append (nrevL x) (C a N)}")
  
  
  def apply(n: String): String = defs(n)
  
  implicit def peano(i: Int): Value =
    if(i == 0)
      Ctr("Z", List())
    else
      Ctr("S", List(peano(i-1)))
        
  def list(vs: Value*): Value = 
    (vs :\ Ctr("N", List()))((x, y) => Ctr("C", List(x, y)))
}