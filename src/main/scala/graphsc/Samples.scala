package graphsc

import interpretation._

object Samples {
  
  val defs = collection.mutable.Map[String, String]()
  
  private def p(s: String, pairs: (List[Value], Value)*) {
    defs += s.takeWhile(_ != ' ') -> s
  }
  
  p("const x y = x")
  p("add x y = case x of { Z -> y; S x -> S (add x y) }")
  p("mul x y = case x of { Z -> Z; S x -> add y (mul x y) }")
  p("padd x y = case x of { Z -> y; S x -> S (padd y x) }")
  p("pmul x y = case x of { Z -> Z; S x -> padd y (pmul y x) }")
  p("id x = case x of {Z -> Z; S x -> S (id x)}")
  p("constz x = case x of {Z -> Z; S x -> constz x}")
  p("reverseTo x y = case x of {Z -> y; S x -> reverseTo x (S y)}")
  p("nrev x = case x of {Z -> Z; S x -> add (nrev x) (S Z)}")
  p("fac x = case x of {Z -> S Z; S x -> mul (S x) (fac x)}")
  p("fib x = case x of {Z -> Z; S x -> case x of {Z -> S Z; S x -> add (fib (S x)) (fib x)}}")
  p("append x y = case x of {N -> y; C a x -> C a (append x y)}")
  p("nrevL x = case x of {N -> N; C a x -> append (nrevL x) (C a N)}")
  p("add3Left x y z = add (add x y) z")
  p("add3Right x y z = add x (add y z)")
  p("ackermann m n = " +
  		"case m of {" +
  		  "Z -> S n;" +
  		  "S k -> case n of {" +
                  "Z -> ackermann k (S Z); " +
                  "S n1 -> ackermann k (ackermann m n1) }}")
  
  
  def get(n: String*): String = n.map(defs(_)).mkString(";")
  
  def apply(n: String*): (String, String) = (n(0), get(n:_*))
  
  implicit def peano(i: Int): Value =
    if(i == 0)
      Ctr("Z", List())
    else
      Ctr("S", List(peano(i-1)))
      
  def list(vs: Value*): Value = 
    (vs :\ Ctr("N", List()))((x, y) => Ctr("C", List(x, y)))
}