package graphsc

import residualization._

class TooManyNodesException(s: String) extends Exception(s)

trait HyperLogger extends Hypergraph {
  abstract override def addHyperedge(h: Hyperedge): RenamedNode = {
    println("\nhyper " + h)
    val n = super.addHyperedge(h)
    println("=> " + n + "\n")
    n
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    println("    new " + h)
    super.onNewHyperedge(h)
  }
  
  abstract override def newNode(a: Set[Int]): RenamedNode = {
    val n = super.newNode(a)
    println("    new node " + n)
    n
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    println("    glue " + l + " = " + r)
    super.beforeGlue(l, r)
  }
  
  override def afterGlue(n: Node) {
    println("    glued " + n)
    super.afterGlue(n)
  }
  
  override def onUsedReduced(n: Node) {
    println("used reduced: " + n)
    super.onUsedReduced(n)
  }
}


object Test {
  def limitDepth(g: DepthTracker with TransformManager, d: Int): (Hyperedge, Hyperedge, String) => Boolean = {
    (h1,h2,name) =>
      if(g.allNodes.size > 50)
        throw new TooManyNodesException("")
      if(g.depths(h1.source.node) >= d) {
        println("\t" + h1 + "\n\t" + h2 + "\n")
        println("Nodes: " + g.allNodes.size + " hypers: " + g.allHyperedges.size)
        true
      }
      else {
        println("*** " + name + " ***")
        println("\t" + h1 + "\n\t" + h2 + "\n")
        println("Nodes: " + g.allNodes.size + " hypers: " + g.allHyperedges.size)
        false
      }
  }
  
  def transAll(g: Transformations with DepthTracker with TransformManager): (Hyperedge, Hyperedge) => Unit = {
    import g._
    TransformationsToProcessor(limitDepth(g, 5),
      "letVar" -> letVar,
      "letLet" -> letLet,
      "letCaseOf" -> letCaseOf,
      "letOther" -> letOther,
      //"caseReduce" -> caseReduce(false),
      "caseVar" -> caseVar,
      "caseCase" -> caseCase,
      "caseTick" -> caseTick
    )
  }
        
  def main(args: Array[String]) {
    val g = new TheHypergraph
        //with HyperTester
        //with Canonizer
        with NamedNodes
        with Transformations
        with TransformManager
        with Prettifier
        //with Visualizer 
        with HyperTester
        with HyperLogger 
        with DepthTracker
        with IntegrityCheckEnabled
        //with OnTheFlyTesting
    
    implicit def peano(i: Int): Value =
      if(i == 0)
        Ctr("Z", List())
      else
        Ctr("S", List(peano(i-1)))
        
    def list(vs: Value*): Value = 
      (vs :\ Ctr("N", List()))((x, y) => Ctr("C", List(x, y)))
    
    val p = new ExprParser(g)
    //p("const x y = x")
    p("add x y = case x of { Z -> y; S x -> S (add x y) }")
    //assert(g.runNode(g("add"), List(2, 3)) == peano(5))
    
    //p("add3Left x y z = add (add x y) z")
    //assert(g.runNode(g("add3Left"), List(3, 2, 1)) == peano(6))
    //p("add3Right x y z = add x (add y z)")
    //assert(g.runNode(g("add3Right"), List(3, 2, 1)) == peano(6))
    
    //p("id x = case x of {Z -> Z; S x -> S (id x)}")
    //assert(g.runNode(g("id"), List(3)) == peano(3))
    p("nrev x = case x of {Z -> Z; S x -> add (nrev x) (S Z)}")
    assert(g.runNode(g("nrev"), List(2)) == peano(2))
    //p("fib x = case x of {Z -> Z; S x -> case x of {Z -> S Z; S x -> add (fib (S x)) (fib x)}}")
    //assert(g.runNode(g("fib"), List(6)) == peano(8))
    
    //p("mul x y = case x of { Z -> Z; S x -> add y (mul x y) }")
    //assert(g.runNode(g("mul"), List(2, 3)) == peano(6))
    /*p("fac x = case x of {Z -> S Z; S x -> mul (S x) (fac x)}")
    assert(g.runNode(g("fac"), List(4)) == peano(24))*/
    
    /*p("padd x y = case x of { Z -> y; S x -> S (padd y x) }")
    assert(g.runNode(g("padd"), List(2, 3)) == peano(5))
    p("pmul x y = case x of { Z -> Z; S x -> padd y (pmul y x) }")
    assert(g.runNode(g("pmul"), List(2, 3)) == peano(6))*/
    
    /*p("append x y = case x of {N -> y; C a x -> C a (append x y)}")
    p("nrevL x = case x of {N -> N; C a x -> append (nrevL x) (C a N)}")
    assert(g.runNode(g("nrevL"), List(list(1,2,3,4))) == list(4,3,2,1))*/
    
    //p("two x = S (case (two x) of {Z -> Z; S x -> S Z})")
    
    //p("idle x = case x of {Z -> Z; S x -> idle (idle x)}")
    //p("constz x = case x of {Z -> Z; S x -> constz x}")
    
    //p("deepseq x y = case x of {Z -> y; S x -> deepseq x y}")
    //p("strictadd1 x y = deepseq x (deepseq y (add x y))")
    //p("strictadd2 x y = deepseq x (deepseq y (add y x))")
    
    //g.updateDepth(g("idle").node, 0)
    //g.updateDepth(g("constz").node, 0)
    //g.updateDepth(g("strictadd1").node, 0)
    //g.updateDepth(g("strictadd2").node, 0)
    //g.updateDepth(g("add3Left").node, 0)
    //g.updateDepth(g("add3Right").node, 0)
    //g.updateDepth(g("id").node, 0)
    g.updateDepth(g("nrev").node, 0)
    //g.updateDepth(g("nrevL").node, 0)
    //g.updateDepth(g("pmul").node, 0)
    //g.updateDepth(g("mul").node, 0)
    
    {
      val out = new java.io.FileWriter("init.dot")
      out.write(g.toDot)
      out.close
    }
    
    try {
      while(g.updatedHyperedges.nonEmpty) {
        println("nodes: " + g.allNodes.size)
        g.transform(transAll(g))
      }
      /*println("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE")
      readLine()
      g.updateAll()
      while(g.updatedHyperedges.nonEmpty) {
        println("nodes: " + g.allNodes.size)
        g.transform(transAll(g))
      }*/
    } catch { 
      case _:TooManyNodesException =>
        println("\n\n\nTOO MANY NODES!!!!!!!!!!!!!!!!!\n\n\n")
    }
     
    println("**********************************************")
    //g.writeDotFrames
    
    
    g.statistics()
    
    
    val out = new java.io.FileWriter("test.dot")
    out.write(g.toDot)
    out.close
    
    println("\n\n")
    
    val like =
      for(l <- g.allNodes; r <- g.allNodes; if l != r; 
          lkl <- LikenessCalculator[Int].likenessN(l, r)) yield {
        val List(l1,r1) = List(l,r).sortBy(_.hashCode())
        (lkl,l1,r1)
      }
    
    for(((i,ren),l,r) <- like.toList.sortBy(-_._1._1) if i > 0 && l.deref.node != r.deref.node) {
      val lpretty = l.prettyDebug
      val rpretty = r.prettyDebug
      val eq = EquivalenceProver().prove(l.deref.node, r.deref.node)
      if(eq != None) {
        /*{
          val out = new java.io.FileWriter("proof.dot")
          out.write("digraph Proof {\n")
          out.write(eq.get.toDot)
          out.write("}")
          out.close
          throw new Exception("")
        }*/
        
        eq.get.performGluing(g)
        println("================================")
        println((i,ren))
        println(lpretty)
        println(rpretty)
        println(eq)
      }
    }
    
    g.statistics()
    
    {
      val out = new java.io.FileWriter("test2.dot")
      out.write(g.toDot)
      out.close
    }
    
    //println(g("strictadd1").node == g("strictadd2").node)
    println(g("idle").node == g("constz").node)
    println(g("add3Left").node == g("add3Right").node)
    
    val resg = new Object with TheHypergraph
    val tf = new TerminationFilter(resg)
    val Some(filterednode) = tf(g("add3Left"))
    
    {
      val out = new java.io.FileWriter("resid.dot")
      out.write(resg.toDot)
      out.close
    }
    
    
    println(g("add3Left").used)
    println(HyperRunner.run(filterednode, List(peano(2),peano(4),peano(3))))
    println(g.runNode(g("add3Left"), List(peano(2),peano(4),peano(3))))
    
    assert(HyperRunner.run(filterednode, List(peano(2),peano(4),peano(3))) == 
           g.runNode(g("add3Left"), List(peano(2),peano(4),peano(3))))
  }
}
