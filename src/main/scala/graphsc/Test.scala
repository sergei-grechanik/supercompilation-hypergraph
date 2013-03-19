package graphsc

import interpretation._
import transformation._
import residualization._

object Trace {
  var suppressedFrom: Int = Int.MaxValue
  var indent: Int = 0
  
  def enter(): this.type = {
    indent += 2
    this
  }
  
  def enterIf(cond: Boolean, header: String = "") = {
    if(cond)
      enter(header)
    else {
      enter
      if(suppressedFrom > indent)
        suppressedFrom = indent
      this
    }
  }
  
  def leave = {
    indent -= 2
    if(suppressedFrom > indent)
      suppressedFrom = Int.MaxValue
    this
  }
  
  def enter(header: String): this.type = {
    this(header)
    enter
  }
  
  def ->:[T](value: T): T = {
    if(suppressed)
      leave
    else {
      leave
      this("> " + value)
    }
    value
  }
  
  def suppressed: Boolean =
    indent >= suppressedFrom
  
  def apply(s: String) = {
    if(!suppressed)
      println(("" /: List.fill(indent)(" "))(_ + _) + s)
    this
  }
}

class TooManyNodesException(s: String) extends Exception(s)

trait HyperLogger extends Hypergraph {
  abstract override def addHyperedge(h: Hyperedge) {
    println("\nhyper " + h)
    super.addHyperedge(h)
    println("=> " + normalize(h) + "\n")
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

trait SelfLetAdder extends Hypergraph {
  var var0: RenamedNode = null
  var creating = false
  override def onNewNode(n: Node) {
    super.onNewNode(n)
    if(var0 != null)
      add(Let(), n.deref, List(var0, n.deref))
    else if(creating != true) {
      creating = true
      var0 = variable(0)
      creating = false
    }
  }
}

object Test {
  
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000 + "ms")
    result
  }
  
  def myLimit(g: Hypergraph, maxnodes: Int = 1000): (Hyperedge,Hyperedge) => Boolean = {
    (h1,h2) =>
      if(g.allNodes.size > maxnodes)
        throw new TooManyNodesException("")
      else {
        println("Nodes: " + g.allNodes.size)
        println(h1)
        println(h2)
        true
      }
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
        //with HyperLogger 
        with DepthTracker
        //with IntegrityCheckEnabled
        //with OnTheFlyTesting
        with SelfLetAdder {
          override def nodeDotLabel(n: Node): String =
            super.nodeDotLabel(n) + "\\l" + depths(n) + " " + codepths(n) + "\\l"
            
          override def onNewNode(n: Node) {
            //assert(n.used.size <= 3)
            super.onNewNode(n)
          }
        }
    
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
    g.zeroBoth(g("add"))
    //assert(g.runNode(g("add"), List(2, 3)) == peano(5))
    
    //p("add3Left x y z = add (add x y) z")
    //g.zeroBoth(g("add3Left"))
    //assert(g.runNode(g("add3Left"), List(3, 2, 1)) == peano(6))
    //p("add3Right x y z = add x (add y z)")
    //g.zeroBoth(g("add3Right"))
    //assert(g.runNode(g("add3Right"), List(3, 2, 1)) == peano(6))
    
    //p("even x = case x of {Z -> T; S x -> odd x}")
    //p("odd x = case x of {Z -> F; S x -> even x}")
    //g.zeroBoth(g("even"))
    //g.zeroBoth(g("odd"))
    
    //p("doubleTo x y = case x of {Z -> y; S x -> doubleTo x (S (S y))}")
    //g.zeroBoth(g("doubleTo"))
    
    //p("even_double x = even (doubleTo x Z)")
    //g.zeroBoth(g("even_double"))
    
    p("revto x y = case x of {Z -> y; S x -> revto x (S y)}")
    p("rev x = case x of {Z -> Z; S x -> revto x (S Z)}")
    g.zeroBoth(g("rev"))
    //assert(g.runNode(g("rev"), List(3)) == peano(3))
    
    //p("id x = case x of {Z -> Z; S x -> S (id x)}")
    //assert(g.runNode(g("id"), List(3)) == peano(3))
    p("nrev x = case x of {Z -> Z; S x -> add (nrev x) (S Z)}")
    g.zeroBoth(g("nrev"))
    assert(g.runNode(g("nrev"), List(3)) == peano(3))
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
    //g.updateDepth(g("nrevL").node, 0)
    //g.updateDepth(g("pmul").node, 0)
    //g.updateDepth(g("mul").node, 0)
    
    
    //p.assume("forall x y . nrevto_plus_1 x y = nrevto x (S y)")
    //val (_,(_,hy1)) = p.assume("forall x y . add_plus_1 x y = add x (S y)")
    //val (_,(_,hy2)) = p.assume("forall x y . nrevto_plus_1 x y = add_plus_1 (rev x) y")
    
    //for((h1,h2) <- TransformationsToProcessor.transformablePairs(g.normalize(hy2),g.normalize(hy1)))
    //  g.letLet((h1,h2))
    
    //p.assume("forall x y . nrevto x y = case x of {Z -> y; S x -> nrevto_plus_1 x y}")
    
    
    //assert(g.runNode(g("add_plus_1"), List(2,3)) == peano(6))

    //p("nrevto x y = add (nrev x) y")
   
    //p("nrevto_plus_1 x y = add (add (nrev x) (S Z)) y")
    
    for(n <- g.allNodes)
      g.zeroBoth(n.deref)
   
    p("nrevto x y = add (nrev x) y")
   
    p("nrevto_plus_1 x y = add (add (nrev x) (S Z)) y")
    
    p("nrevto_1 x = nrevto x (S Z)")
    
    p("add_plus_1 x y = add (add (x) (S (Z ))) ((y))")
    
    p("add_1_to y = add (S Z) y")
    p("add_0_to y = add Z y")
    
    {
    val gresid = new TheHypergraph with Prettifier {}
    val residualizer = new Residualizer(gresid, SCC(g))
    val resid = time(residualizer(g("rev")))
    gresid.setName(resid.get.node.node, "main")
    gresid.removeUnreachable(resid.get.node.node)
    gresid.nameUnnamed()
    println("misses: " + residualizer.misses)
    println("Resid cost: " + resid.get.cost)
    println("Graph size: " + gresid.nodes.size)
    println(gresid.prettyProgram)
    }
    {
    val gresid = new TheHypergraph with Prettifier {}
    val residualizer = new Residualizer(gresid, SCC(g))
    val resid = time(residualizer(g("nrev")))
    gresid.setName(resid.get.node.node, "main")
    gresid.removeUnreachable(resid.get.node.node)
    gresid.nameUnnamed()
    println("misses: " + residualizer.misses)
    println("Resid cost: " + resid.get.cost)
    println("Graph size: " + gresid.nodes.size)
    println(gresid.prettyProgram)
    }
    readLine()
    
    {
      val out = new java.io.FileWriter("init.dot")
      out.write(g.toDot)
      out.close
    }
    
    val buf = HyperBuffer(g)
    val tr = new PostFilter(buf, h => h.arity <= 3) with Transformations
    
    //g.processor = HyperedgePairProcessor(transAll(g))
    //g.updateAll()
    try {
      var stop = false
      while(!stop) {
        while(g.updatedHyperedges.nonEmpty) {
          println("nodes: " + g.allNodes.size)
          g.transform(
              (tr.transDrive & tr.letUp(3).cond(g.limitDepthCodepth((d,c) => c == 0 || c + d <= 1))).cond(
                  g.limitDepth(3) & g.limitCodepth(3) & myLimit(g, 500)).onSuccess(
                      () => { tr.commit(); println("buf size: " + buf.buffer.size) } ))
        }
        buf.commit()
        println("After commit nodes: " + g.nodes.size + " hypers: " + g.allHyperedges.size)
        
        println("nrev = rev, our goal:")
        println(p.check("forall x . nrev x = rev x"))
        
        val parnodes = g.allNodes.toList.par
        val like =
          for(l <- parnodes; r <- parnodes; if l != r && l.hashCode <= r.hashCode; 
              lkl <- LikenessCalculator[Int].likenessN(l, r); if lkl._1 > 0) yield {
            (lkl,l,r)
          }
        
        println("Computed likeness")
        
        
        /*val scc = SCC(g)
        val proofs =
          for(((i,ren),l,r) <- like) yield {
            val eprover = new EquivalenceProver(scc)
            val proof = eprover.prove(l.deref.node, r.deref.node)
            (proof, eprover.stats, l.prettyDebug, r.prettyDebug)
          }
        
        val stats = collection.mutable.Map[(Node, Node), Int]()
        
        for((p, s, lpretty, rpretty) <- proofs.toList) {
          if(p != None) {
            println("=======================")
            println(lpretty)
            println(rpretty)
            println(p)
            p.get.performGluing(g)
          }
          
          for((ns,c) <- s) {
            if(stats.contains(ns))
              stats(ns) += c
            else
              stats(ns) = c  
          }
        }*/
        
        var eprover = new EquivalenceProver(g)
        
        for(((i,ren),l,r) <- like.toList.sortBy(-_._1._1)
              if i > 0 && l.deref.node != r.deref.node) {
          val lpretty = l.prettyDebug
          val rpretty = r.prettyDebug
          val eq = eprover.prove(l.deref.node, r.deref.node)
          if(eq != None) {
            println("=======================")
            println((i,ren))
            println(lpretty)
            println(rpretty)
            println(eq)
            eq.get.performGluing(g)
            val st = eprover.stats
            eprover = new EquivalenceProver(g)
            eprover.stats = st
          }
        }
        
        val stats = eprover.stats
        
        for(((l1,r1),c) <- stats.toList.sortBy(_._2)) {
          val l = l1.deref.node
          val r = r1.deref.node
          val lik = LikenessCalculator[Int].likeness(l.deref, r.deref)
          if(lik != None && lik.get._1 == 0 ) {
            println(c + " $$$$$$$ This would help $$$$$$$$$ " + lik)
            println(l.prettyDebug)
            println("---")
            println(r.prettyDebug)
            println("^^^^^^^^^^")
            g.zeroBoth(l.deref)
            g.zeroBoth(r.deref)
            if(g.drive(l) == None)
              println("seems undrivable\n" + l.prettyDebug)
            if(g.drive(r) == None)
              println("seems undrivable\n" + r.prettyDebug)
          }
        }
        
        /*val nrt = g("nrevto")
        println("nrevto: " + g.depth(nrt) + " " + g.codepth(nrt))
        
        println("nrevto driven:")
        println(p.check("forall x y . nrevto x y = case x of {Z -> y; S x -> nrevto_plus_1 x y}"))
        println("add_plus_1 simplified:")
        println(p.check("forall x y . add_plus_1 x y = add x (S y)"))
        println("nrevto_plus_1 in terms of add_plus_1:")
        println(p.check("forall x y . nrevto_plus_1 x y = add_plus_1 (rev x) y"))
        println("nrevto_plus_1 in terms of nrevto:")
        println(p.check("forall x y . nrevto_plus_1 x y = nrevto x (S y)"))
        println("nrev = rev, our goal:")
        println(p.check("forall x . nrev x = rev x"))*/
        
        
        println("After eqproof: " + g.nodes.size + " hypers: " + g.allHyperedges.size)
        
        {
        val gresid = new TheHypergraph with Prettifier {}
        val residualizer = new Residualizer(gresid, SCC(g))
        val resid = time(residualizer(g("rev")))
        gresid.setName(resid.get.node.node, "main")
        gresid.removeUnreachable(resid.get.node.node)
        gresid.nameUnnamed()
        println("misses: " + residualizer.misses)
        println("Resid cost: " + resid.get.cost)
        println("Graph size: " + gresid.nodes.size)
        println(gresid.prettyProgram)
        }
        {
        val gresid = new TheHypergraph with Prettifier {}
        val residualizer = new Residualizer(gresid, SCC(g))
        val resid = time(residualizer(g("nrev")))
        gresid.setName(resid.get.node.node, "main")
        gresid.removeUnreachable(resid.get.node.node)
        gresid.nameUnnamed()
        println("misses: " + residualizer.misses)
        println("Resid cost: " + resid.get.cost)
        println("Graph size: " + gresid.nodes.size)
        println(gresid.prettyProgram)
        }
        
        if(readLine().startsWith("s"))
          stop = true
      }
      println("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE")
    } catch { 
      case _:TooManyNodesException =>
        println("\n\n\nTOO MANY NODES!!!!!!!!!!!!!!!!!\n\n\n")
    }
    
    
    /*
    try {
      g.updateAll()
      while(g.updatedHyperedges.nonEmpty) {
        println("nodes: " + g.allNodes.size)
        g.transform(transDrive(g))
      }
    } catch { 
      case _:TooManyNodesException =>
        println("\n\n\nTOO MANY NODES!!!!!!!!!!!!!!!!!\n\n\n")
    }*/
     
    println("**********************************************")
    //g.writeDotFrames
    
    /*for(n <- g.allNodes)
      if(g.depths(n) + g.codepths(n) > 6)
        g.removeNode(n)*/
    
    //for(n <- g.allNodes)
    //  g.drive(n.deref.node)
    
      
    g.statistics()
    
    {
      val out = new java.io.FileWriter("test.dot")
      out.write(g.toDot)
      out.close
    }
    
    {
      val out = new java.io.FileWriter("rough.dot")
      out.write("digraph Rough {\n")
      for(n <- g.allNodes) {
        out.write("\"" + n + "\"[label=\"" + n.prettyDebug.replace("\n", "\\l") + "\\l" + "\"];\n")
        for(o <- n.outs; d <- o.dests)
          out.write("\"" + n + "\" -> \"" + d.node + "\";\n")
      }
      out.write("}")
      out.close
    }
    
    {
      val out = new java.io.FileWriter("depths")
      for(n <- g.allNodes) {
        out.write(n.used.size + " " + g.depth(n.deref) + " " + g.codepth(n.deref) + " " + g.pretty(n).size + " " +
            g.pretty(n).replace("\n", "\t") + "\n")
      }
      out.close()
    }
    
    {
      val out = new java.io.FileWriter("hypers")
      for(h <- g.allHyperedges) {
        out.write(h.label + " " + h.dests.size + " " + h.source.arity + " " + h.arity + " " +
            h.dests.map(_.arity).mkString(" ") + "\n")
      }
      out.close()
    }
    
    //assert(g.runNode(g("nrev"), List(5)) == peano(5))
    assert(g.runNode(g("add_plus_1"), List(2,3)) == peano(6))
    println("ok")
    g.statisticsTester()
    
    println("nrevto driven:")
    println(p.check("forall x y . nrevto x y = case x of {Z -> y; S x -> nrevto_plus_1 x y}"))
    println("add_plus_1 simplified:")
    println(p.check("forall x y . add_plus_1 x y = add x (S y)"))
    println("nrevto_plus_1 in terms of add_plus_1:")
    println(p.check("forall x y . nrevto_plus_1 x y = add_plus_1 (rev x) y"))
    println("nrevto_plus_1 in terms of nrevto:")
    println(p.check("forall x y . nrevto_plus_1 x y = nrevto x (S y)"))
    
    println("[][][][][][][][[][[][][][][][][][][][][]")
    //println(EquivalenceProver().prove(g("revto").deref.node, g("nrevto").deref.node))
    println("[][][][][][][][[][[][][][][][][][][][][]")
    
    
    val like =
      for(l <- g.allNodes; r <- g.allNodes; if l != r; 
          lkl <- LikenessCalculator[Int].likenessN(l, r)) yield {
        val List(l1,r1) = List(l,r).sortBy(_.hashCode())
        (lkl,l1,r1)
      }
    
    for(((i,ren),l,r) <- like.toList.sortBy(-_._1._1) if i > 0 && l.deref.node != r.deref.node) {
      val lpretty = l.prettyDebug
      val rpretty = r.prettyDebug
      val eq = (new EquivalenceProver(g)).prove(l.deref.node, r.deref.node)
      if(i > 2 || eq != None) {
        println("=======================")
        println((i,ren))
        println(lpretty)
        println(rpretty)
        println(eq)
      }
      if(eq != None) {
        {
          val out = new java.io.FileWriter("proof.dot")
          out.write("digraph Proof {\n")
          out.write(eq.get.toDot)
          out.write("}")
          out.close
        }
        
        eq.get.performGluing(g)
      }
    }
    
    println("add_plus_1 simplified:")
    println(p.check("forall x y . add_plus_1 x y = add x (S y)"))
    
    g.statistics()
    
    {
      val out = new java.io.FileWriter("test2.dot")
      out.write(g.toDot)
      out.close
    }
    
    println(g("nrev").node == g("rev").node)
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
