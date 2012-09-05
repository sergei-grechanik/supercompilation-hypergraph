package graphsc

class TooManyNodesException(s: String) extends Exception(s)

trait Visualizer extends TheHypergraph {
  val dotObjects = collection.mutable.Map[String, String]()
  
  var frames: List[Set[String]] = Nil
  
  def save(e: Any = null) {
    frames ::= dotVisible
  }
  
  def writeDotFrames() {
    for((s,i) <- frames.reverse.zipWithIndex) {
      val out = new java.io.FileWriter("test" + i + ".dot")
      out.write(dotVisualize(s))
      out.close
    }
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    dotHyperedge(h)
    save()
    super.onNewHyperedge(h)
  }
  
  override def afterGlue(n: Node) {
    for(h <- n.ins ++ n.outs)
      dotHyperedge(h)
    save()
    super.afterGlue(n)
  }
  
  def dotHyperedge(h: Hyperedge) {
    dotObjects += ("\"" + h.source.uniqueName + "\"") -> "label=\"\",shape=circle"
    dotObjects += ("\"" + h + "\"") -> 
      ("shape=record,label=\"{" + h.label.toString + "|{" + 
            (0 until h.dests.length).map("<" + _ + ">").mkString("|") + "}}\"")
    dotObjects += ("\"" + h.source.uniqueName + "\" -> \"" + h + "\"") -> "label=\"\""
    for((d,i) <- h.dests.zipWithIndex)
          dotObjects += ("\"" + h + "\":" + i + " -> \"" + d.uniqueName + "\"") -> "label=\"\""
  }
  
  def dotVisible: Set[String] = {
    val s = collection.mutable.Set[String]()
    for(n <- nodes) {
      s += "\"" + n.uniqueName + "\""
      for(h <- n.ins ++ n.outs) {
        s += "\"" + h + "\""
        s += "\"" + h.source.uniqueName + "\" -> \"" + h + "\""
        for((d,i) <- h.dests.zipWithIndex)
          s += "\"" + h + "\":" + i + " -> \"" + d.uniqueName + "\""
      }
    }
    s.toSet
  }
  
  def dotVisualize(objs: Set[String]): String = {
    val s = new StringBuilder
    s.append("digraph Hyper {\n")
    for((n,p) <- dotObjects) {
      val c =
        if(objs(n))
          ",color=black"
        else
          ",color=white,fontcolor=white"
      s.append(n + "[" + p + c + "];\n")
    }
    s.append("}")
    s.toString
  }
}

trait HyperLogger extends Hypergraph {
  abstract override def addHyperedge(h: Hyperedge): Node = {
    println("\nhyper " + h)
    val n = super.addHyperedge(h)
    println("=> " + n + "\n")
    n
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    println("    new " + h)
    super.onNewHyperedge(h)
  }
  
  abstract override def newNode(a: Int): Node = {
    val n = super.newNode(a)
    println("    new node " + n)
    n
  }
  
  override def beforeGlue(l: Node, r: Node) {
    println("    glue " + l.uniqueName + " " + r.uniqueName)
    super.beforeGlue(l, r)
  }
}


object Test {
  def limitNodes(g: Hypergraph, l: Int): (Hyperedge, Hyperedge, String) => Unit = {
    val init = g.allNodes.size
    (h1,h2,name) =>
      println("*** " + name + " ***")
      println("\t" + h1 + "\n\t" + h2 + "\n")
      println("Nodes: " + g.allNodes.size + " limit: " + (init + l))
      if(g.allNodes.size > init + l)
        throw new TooManyNodesException("")
  }
  
  def transAll(g: Transformations): (Hyperedge, Hyperedge) => Unit = {
    import g._
    TransformationsToProcessor(limitNodes(g, 20),
      "renamingVar" -> renamingVar,
      "letVar" -> letVar,
      "letLet" -> letLet,
      "letCaseOf" -> letCaseOf,
      "letOther" -> letOther,
      "caseReduce" -> caseReduce,
      "caseVar" -> caseVar,
      "caseCase" -> caseCase,
      "letRenaming" -> letRenaming,
      "renamingRenaming" -> renamingRenaming,
      "anyRenaming" -> anyRenaming
    )
  }
  
  def transReduce(g: Transformations): (Hyperedge, Hyperedge) => Unit = {
    import g._
    TransformationsToProcessor(limitNodes(g, 20),
        "renamingRenaming" -> renamingRenaming,
        "renamingVar" -> renamingVar,
        "letVar" -> letVar,
        //"letLet" -> letLet,
        //"letCaseOf" -> letCaseOf,
        //"letOther" -> letOther,
        //"caseReduce" -> caseReduce,
        //"caseVar" -> caseVar,
        //"caseCase" -> caseCase,
        "letRenaming" -> letRenaming,
        "letVar" -> letVar
        //"anyRenaming" -> anyRenaming
    )
  }
  
  def transLetRenaming(g: Transformations): (Hyperedge, Hyperedge) => Unit = {
    import g._
    TransformationsToProcessor(limitNodes(g, 20),
        "letRenaming" -> letRenaming)
  }
  
        
  def main(args: Array[String]) {
    val g = new TheHypergraph
        with Canonizer
        with NamedNodes
        with Transformations
        with TransformManager
        with Prettifier
        //with Visualizer 
        //with HyperTester
        with HyperLogger 
    
    implicit def peano(i: Int): Value =
      if(i == 0)
        Ctr("Z", List())
      else
        Ctr("S", List(peano(i-1)))
        
    def list(vs: Value*): Value = 
      (vs :\ Ctr("N", List()))((x, y) => Ctr("C", List(x, y)))
    
    val p = new ExprParser(g)
    //p("fst x y = x")
    //p("snd x y = y")
    //p("ololo x y = snd (fst y x) (fst (snd (fst x y) y) x)")
    //assert(g.runNode(g("ololo"), List(1, 2)) == peano(2))
    p("add x y = case x of { Z -> y; S x -> S (add x y) }")
    //assert(g.runNode(g("add"), List(2, 3)) == peano(5))
    /*p("mul x y = case x of { Z -> Z; S x -> add y (mul x y) }")
    assert(g.runNode(g("mul"), List(2, 3)) == peano(6))
    p("padd x y = case x of { Z -> y; S x -> S (padd y x) }")
    assert(g.runNode(g("padd"), List(2, 3)) == peano(5))
    p("pmul x y = case x of { Z -> Z; S x -> padd y (pmul y x) }")
    assert(g.runNode(g("pmul"), List(2, 3)) == peano(6))
    p("id x = case x of {Z -> Z; S x -> S (id x)}")
    assert(g.runNode(g("id"), List(3)) == peano(3))
    p("nrev x = case x of {Z -> Z; S x -> add (nrev x) (S Z)}")
    assert(g.runNode(g("nrev"), List(2)) == peano(2))
    p("fac x = case x of {Z -> S Z; S x -> mul (S x) (fac x)}")
    assert(g.runNode(g("fac"), List(4)) == peano(24))
    p("fib x = case x of {Z -> Z; S x -> case x of {Z -> S Z; S x -> add (fib (S x)) (fib x)}}")
    assert(g.runNode(g("fib"), List(6)) == peano(8))
    p("append x y = case x of {N -> y; C a x -> C a (append x y)}")
    p("nrevL x = case x of {N -> N; C a x -> append (nrevL x) (C a N)}")
    assert(g.runNode(g("nrevL"), List(list(1,2,3,4))) == list(4,3,2,1))*/
    
    {
      val out = new java.io.FileWriter("init.dot")
      out.write(g.toDot)
      out.close
    }
    
    try {
      for(i <- 0 to 50) {
        println("nodes: " + g.allNodes.size)
        g.transform(transAll(g))
      }
    } catch { 
      case _:TooManyNodesException =>
        println("\n\n\nTOO MANY NODES!!!!!!!!!!!!!!!!!\n\n\n")
    }
    try {
      g.statistics()
      readLine()
      g.updateAll()
      for(i <- 0 to 10) {
        println("OLOLO: " + g.allNodes.size)
        g.statistics()
        g.transform(transReduce(g))
      }
      g.statistics()
      /*readLine()
      g.updateAll()
      g.transform(transLetRenaming(g))*/
    } catch {
      case _:TooManyNodesException =>
        println("aborted")
        //g.statistics()
    }
     
    println("**********************************************")
    //g.writeDotFrames
    
    g.statistics()
    
    val out = new java.io.FileWriter("test.dot")
    out.write(g.toDot)
    out.close
  }
}
