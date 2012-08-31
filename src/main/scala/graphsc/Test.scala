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

trait Transformer extends HyperTester with Transformations with Prettifier {
  val stat = collection.mutable.Map[String, List[Int]]()
  val updatedHyperedges = collection.mutable.Set[Hyperedge]()
  
  def allHyperedges: Set[Hyperedge] = {
    val sets = allNodes.toList.map(n => n.ins ++ n.outs)
    (Set[Hyperedge]() /: sets)(_ | _)
  }
  
  override def onNewHyperedge(h: Hyperedge) {    
    updatedHyperedges += h
    super.onNewHyperedge(h)
  }
  
  override def afterGlue(n: Node) {
    updatedHyperedges ++= n.outs
    super.afterGlue(n)
  }
  
  def transforming(hs: Hyperedge*) {
    statistics()
    for(h <- hs)
      println("    " + h)
  }
  
  def transform(
        h: Hyperedge, 
        t: PartialFunction[Hyperedge, List[Hyperedge]], 
        name: String) {
    t.lift(h) match {
      case Some(l) =>
        println(name)
        transforming(h)
        for(nh <- l)
          addHyperedge(nh)
      case None =>
    }
  }
  
  def transform(
        h: Hyperedge, 
        t: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]],
        name: String)
  (implicit dummy1: DummyImplicit) {
    for(d <- h.dests; h1 <- d.outs) {
      t.lift(h, h1) match {
        case Some(l) =>
          println(name)
          transforming(h, h1)
          for(nh <- l)
            addHyperedge(nh)
        case None =>
      }
    }
  }
  
  def transform(h1: Hyperedge, h2: Hyperedge, simple: Boolean) {
    val tr =
      if(simple)
      List(
        "renamingVar" -> renamingVar,
        "letVar" -> letVar,
        //"letLet" -> letLet,
        //"letCaseOf" -> letCaseOf,
        //"letOther" -> letOther,
        //"caseReduce" -> caseReduce,
        //"caseVar" -> caseVar,
        //"caseCase" -> caseCase,
        //"letRenaming" -> letRenaming,
        "renamingRenaming" -> renamingRenaming,
        "anyRenaming" -> anyRenaming
        )
      else
      List(
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
        "anyRenaming" -> anyRenaming)
        
    for((name,trans) <- tr) {
      if(trans.isDefinedAt((h1,h2))) {
        val before = allNodes.size
        println("\ntransforming:")
        println(name)
        transforming(h1, h2)
        println(prettyTwoHyperedges(h1, h2))
        trans((h1,h2))
        val after = allNodes.size
        
        stat += name -> ((after - before) :: stat.getOrElse(name, Nil))
      }
      
      if((!simple && (counter > 300 || allNodes.size > 100)) || allNodes.size > 200)
        throw new TooManyNodesException("")
      
    }
  }
  
  var counter = 0
  def transform(simple: Boolean = false) {
    
    val set = updatedHyperedges.map(_.derefGlued)
    println("***********************")
    println("*** updnhyp: " + set.size)
    println("***********************")
    updatedHyperedges.clear()
    val processed = collection.mutable.Set[Hyperedge]()
    for(h1 <- set; val h = h1.derefGlued; if !processed(h) && !processed(h1)) {
      processed += h
      counter += 1
      println(counter)
      for(d <- h.dests; h1 <- d.outs)
        transform(h, h1, simple)
      for(h1 <- h.source.ins)
        transform(h1, h, simple)
    }
  }
}

trait Canonizer extends TheHypergraph {
  
  def addHyperedgeSuper(h: Hyperedge): Node =
      super.addHyperedge(h)
  
  def newNodeSuper(a: Int): Node =
      super.newNode(a)
      
  object superthis extends Transformations {
    def addHyperedge(h: Hyperedge): Node =
      addHyperedgeSuper(h)
      
    def newNode(a: Int): Node =
      newNodeSuper(a)
  }
  
  override def addHyperedge(h: Hyperedge): Node = {
    superthis.throughRenamings(h)
    h.source.getRealNode
  }
}



object Test {
  def main(args: Array[String]) {
    val g = new TheHypergraph
        //with Canonizer
        with NamedNodes
        with Transformer
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
    //assert(g.runNode(g("ololo"), Vector(1, 2)) == peano(2))
    p("add x y = case x of { Z -> y; S x -> S (add x y) }")
    assert(g.runNode(g("add"), Vector(2, 3)) == peano(5))
    /*p("mul x y = case x of { Z -> Z; S x -> add y (mul x y) }")
    assert(g.runNode(g("mul"), Vector(2, 3)) == peano(6))
    p("padd x y = case x of { Z -> y; S x -> S (padd y x) }")
    assert(g.runNode(g("padd"), Vector(2, 3)) == peano(5))
    p("pmul x y = case x of { Z -> Z; S x -> padd y (pmul y x) }")
    assert(g.runNode(g("pmul"), Vector(2, 3)) == peano(6))
    p("id x = case x of {Z -> Z; S x -> S (id x)}")
    assert(g.runNode(g("id"), Vector(3)) == peano(3))
    p("nrev x = case x of {Z -> Z; S x -> add (nrev x) (S Z)}")
    assert(g.runNode(g("nrev"), Vector(2)) == peano(2))
    p("fac x = case x of {Z -> S Z; S x -> mul (S x) (fac x)}")
    assert(g.runNode(g("fac"), Vector(4)) == peano(24))
    p("fib x = case x of {Z -> Z; S x -> case x of {Z -> S Z; S x -> add (fib (S x)) (fib x)}}")
    assert(g.runNode(g("fib"), Vector(6)) == peano(8))
    p("append x y = case x of {N -> y; C a x -> C a (append x y)}")
    p("nrevL x = case x of {N -> N; C a x -> append (nrevL x) (C a N)}")
    assert(g.runNode(g("nrevL"), Vector(list(1,2,3,4))) == list(4,3,2,1))*/
    try {
      for(i <- 0 to 50) {
        println("nodes: " + g.allNodes.size)
        g.transform()
      }
    } catch { 
      case _:TooManyNodesException => 
        try {
          println("\n\n\nTOO MANY NODES!!!!!!!!!!!!!!!!!\n\n\n")
          readLine()
          g.updatedHyperedges ++= g.allHyperedges
          for(i <- 0 to 10) {
            println("OLOLO: " + g.allNodes.size)
            g.transform(true)
          }
        } catch {
          case _:TooManyNodesException =>
            println("aborted")
            g.statistics()
        }
    } 
    println("**********************************************")
    //g.writeDotFrames
    
    g.statistics()
    for((n, l) <- g.stat)
      println(n + ": " + l.sum.toDouble/l.length + " (" + l.filter(_ < 0).sum + "/" + l.filter(_ > 0).sum + ")")
    
    val out = new java.io.FileWriter("test.dot")
    out.write(g.toDot)
    out.close
  }
}