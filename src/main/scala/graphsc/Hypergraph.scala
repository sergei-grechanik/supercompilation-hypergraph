package graphsc

trait Hypergraph {
  // h should be with known dests and null source
  // if source is not null then perform gluing
  // this function may perform some hyperedge transformations
  def addHyperedge(h: Hyperedge)
    
  // note that this function doesn't copy the node n,
  // so you cannot use n afterwards as a free node
  def addNode(n: Node): Node
  
  def addNode(l: Label, ds: List[Node]): Node = addNode(Node(l, ds))
  def newNode(arity: Int): Node
  def removeNode(n: Node)
  
  // Nodes shouldn't be glued manually, they should be marked equal with 
  // a Renaming() hyperedge. Then the hypergraph should glue these nodes automatically.
  def glueNodes(l: Node, r: Node): Node
  
  def onNewHyperedge(h: Hyperedge) {}
  def beforeGlue(l: Node, r: Node) {}
  def afterGlue(l: Node) {}
  
  def allNodes: Set[Node]
}

trait HypergraphProxy extends Hypergraph {
  def self: Hypergraph
  override def addHyperedge(h: Hyperedge) = self.addHyperedge(h)
  override def addNode(n: Node): Node = self.addNode(n)
  override def addNode(l: Label, ds: List[Node]): Node = self.addNode(l, ds)
  override def newNode(arity: Int): Node = self.newNode(arity)
  override def removeNode(n: Node) = self.removeNode(n)
  override def glueNodes(l: Node, r: Node): Node = self.glueNodes(l, r)
  override def onNewHyperedge(h: Hyperedge) = self.onNewHyperedge(h)
  override def beforeGlue(l: Node, r: Node) = self.beforeGlue(l, r)
  override def afterGlue(l: Node) = self.afterGlue(l)
  override def allNodes: Set[Node] = self.allNodes
}

class TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  override def allNodes: Set[Node] = nodes.toSet
  
  def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    val n = Node(h)
    nodes += n
    val res = h.from(n)
    res.dests.foreach(_.insMut.add(res))
    res
  }
  
  def addHyperedgeImpl(h1: Hyperedge) {
    val Hyperedge(l1, s1, dst1) = h1
    val adder = addNodeP(addHyperedgeP(h => List(h), addHyperedgeImpl _)) _
    val h = Hyperedge(l1, s1, dst1.map(adder)).derefGlued
    
    if(h.dests.nonEmpty)
      h.dests(0).ins.find(x => x.label == h.label && x.dests == h.dests) match {
        case Some(x) if h.source == null => x
        case Some(x) if h.source == x.source => x
        case Some(x) => glueNodes(h.source, x.source)
        case None => 
          val newh = addHyperedgeSimple(h)
          checkIntegrity()
          onNewHyperedge(newh)
      }
    else
      nodes.find(_.outs.exists(_.label == h.label)) match {
        case Some(n) if h.source == null => h.from(n)
        case Some(n) => glueNodes(h.source, n)
        case None => 
          val newh = addHyperedgeSimple(h)
          checkIntegrity()
          onNewHyperedge(newh)
      }
  }
  
  def preprocessHyperedge(h: Hyperedge): List[Hyperedge] = {
    val Hyperedge(l, src, ds) = h
    l match {
      case _ if h.isId =>
        List(Hyperedge(Id(), src, List(ds(0))))
      case Renaming(a, vec) if 
        a == vec.length && vec.zipWithIndex.forall{ case (a,b) => a == b } =>
        // we do both because renamings mark canonicalized nodes
        List(h, Hyperedge(Id(), src, ds))
      case _ =>
        List(h)
    }
  }
  
  def preprocessors: List[Hyperedge => List[Hyperedge]] =
    List(preprocessHyperedge)
  
  override def addHyperedge(h: Hyperedge): Unit =
    addHyperedgeP(preprocessors)(h)
  
  def addHyperedgeP(ps: List[Hyperedge => List[Hyperedge]])(h: Hyperedge) {
    ps match {
      case p :: ps => addHyperedgeP(p, addHyperedgeP(ps) _)(h)
      case Nil => addHyperedgeImpl(h)
    }
  }
    
  def addHyperedgeP(hyperedgePreprocessor: Hyperedge => List[Hyperedge],
                    nextAdder: Hyperedge => Unit)(h1: Hyperedge) {
    // Make sure that all destination nodes are added
    val Hyperedge(l1, s1, dst1) = h1
    val adder = addNodeP(addHyperedgeP(hyperedgePreprocessor, nextAdder) _) _
    val h = Hyperedge(l1, s1, dst1.map(adder)).derefGlued
    
    // Preprocessing may perform some useful transformations like canonicalization
    for(nh <- hyperedgePreprocessor(h)) nh match {
      case Hyperedge(Id(), src, List(dst)) => glueNodes(src, dst)
      case _ => nextAdder(nh)
    }
  }
  
  override def addNode(n: Node): Node =
    addNodeP(addHyperedge _)(n)
  
  def addNodeP(hyperedgeAdder: Hyperedge => Unit)(n: Node): Node = {
    val realn = n.getRealNode
    if(!nodes(realn)) {
      val outs = realn.outs
      // we should remove all connections before adding the node
      // to preserve integrity of the hypergraph
      // the connections will be restored later by hyperedge addition
      realn.outsMut.clear
      realn.insMut.clear
      nodes.add(realn)
      for(h <- outs)
        hyperedgeAdder(h)
    }
    checkIntegrity()
    n.getRealNode
  }
  
  override def newNode(arity: Int): Node = {
    val n = new Node(arity)
    nodes.add(n)
    n
  }
  
  override def removeNode(n: Node) {
    nodes -= n
    // we should leave n.outs and n.ins intact
    // so we remove all hyperedges incident with n
    // from all incident nodes except n
    for(h <- n.ins) {
      if(h.source != n)
        h.source.outsMut -= h
      for(d <- h.dests if d != n)
        d.insMut -= h
    }
    for(h <- n.outs; d <- h.dests if d != n)
      d.insMut -= h
    checkIntegrity()
  }
  
  override def glueNodes(l1: Node, r1: Node): Node = {
    // just for convenience, this feature is undocumented, don't use it
    if(l1 == null) return r1.getRealNode
    if(r1 == null) return l1.getRealNode
    
    if(!nodes.contains(l1.getRealNode))
      addNode(l1.getRealNode)
    if(!nodes.contains(r1.getRealNode))
      addNode(r1.getRealNode)
    
    val l = l1.getRealNode
    val r = r1.getRealNode
    
    if(l != r) {
      assert(l.arity == r.arity)
      // We add temporary id hyperedges, so that HyperTester won't crash
      addHyperedgeSimple(Hyperedge(Id(), l, List(r)))
      addHyperedgeSimple(Hyperedge(Id(), r, List(l)))
      beforeGlue(l, r)
      
      checkIntegrity()
      removeNode(r)
      r.gluedTo = l
      for(h <- r.mouts)
        addHyperedgeSimple(h.derefGlued)
      for(h <- r.mins)
        addHyperedgeSimple(h.derefGlued)
        
      // Remove id nodes
      // Id nodes can only be added by the two lines above,
      // so we don't remove anything we shouldn't
      l.outsMut -= Hyperedge(Id(), l, List(l))
      l.insMut -= Hyperedge(Id(), l, List(l))
        
      afterGlue(l)
      checkIntegrity()
      // maybe there appeared some more nodes to glue 
      glueParents(l)
      // Now l may be glued to something else
      l.getRealNode
    }
    else //if(l == r)
      l // Nodes are already glued
  }
  
  // glue parents recursively
  def glueParents(n: Node) {
    val groups = n.ins.groupBy(h => (h.label, h.dests)).filter(_._2.size > 1)
    for((_, g) <- groups)
      g.toList.map(_.source).reduce(glueNodes)
  }
  
  def nodeDotLabel(n: Node): String =
    n.uniqueName
  
  def toDot: String = {
    val sb = new StringBuilder()
    sb.append("digraph Hyper {\n")
    for(n <- nodes) {
      sb.append("\"" + n.uniqueName + "\"[label=\"" + nodeDotLabel(n) + "\", shape=box];\n")
      for(h <- n.outs) {
        val lab = "{" + h.label.toString + "|{" + 
            (0 until h.dests.length).map("<" + _ + ">").mkString("|") + "}}"
        sb.append("\"" + h.toString + "\"[label=\"" + lab + "\", shape=record];\n")
        sb.append("\"" + n.uniqueName + "\" -> \"" + h.toString + "\";\n")
        for((d,i) <- h.dests.zipWithIndex)
          sb.append("\"" + h.toString + "\":" + i + " -> \"" + d.uniqueName + "\";\n")
      }
      sb.append("\n")
    }
    sb.append("}\n")
    sb.toString
  }
  
  def checkIntegrity() {
    return
    for(n <- nodes) {
      assert(n.getRealNode == n)
      for(h <- n.ins) {
        assert(nodes(h.source))
        assert(h.dests.forall(nodes(_)))
        assert(h.source.outs(h))
        assert(h.dests.forall(_.ins(h)))
      }
      for(h <- n.outs) {
        assert(nodes(h.source))
        assert(h.dests.forall(nodes(_)))
        assert(h.dests.forall(_.ins(h)))
        assert(h.source == n)
      }
    }
  }
}

// Just supports named nodes
trait NamedNodes extends Hypergraph {
  val namedNodes = collection.mutable.Map[String, Node]()
  
  def apply(n: String): Node = namedNodes(n)
  
  def addNode(n: String, arity: Int): Node = 
    if(namedNodes.contains(n)) {
      namedNodes(n)
    }
    else {
      val node = newNode(arity)
      namedNodes += n -> node
      node
    }
}

class NonTerminationException(s: String = "") extends Exception(s)

class RunningContext {
  val visited = collection.mutable.Set[(Node, Vector[Value])]()
  val failed = collection.mutable.Set[(Hyperedge, Vector[Value])]()
}

trait HyperTester extends TheHypergraph {
  val runCache = collection.mutable.Map[(Node, Vector[Value]), Value]()
  
  def runNode(n: Node, args: Vector[Value]): Value = {
    val ctx = new RunningContext
    val res = runNode(ctx, n, args)
    checkFailed(ctx)
    res
  }
  
  def runNode(ctx: RunningContext, n: Node, args: Vector[Value]): Value = 
    runCache.get((n,args)) match {
      case Some(v) =>
        //println(n.uniqueName + "(" + args + ") = " + v)
        v
      case None => 
        val res = runNodeUncached(ctx, n, args)
        //println(n.uniqueName + "(" + args + ") = " + res)
        res
    }
    
  def runNodeUncached(ctx: RunningContext, n: Node, args: Vector[Value]): Value = {
    if(ctx.visited((n,args)))
      throw new NonTerminationException("Nontermination detected")
    
    ctx.visited.add((n,args))
    var v: Value = null
    // Try id hyperedges first
    val outs = 
      n.outs.filter(_.label.isInstanceOf[Id]).toList ++
      n.outs.filter(!_.label.isInstanceOf[Id]).toList
    for(h <- outs)
      try {
        val newv = runHyperedgeUncached(ctx, h, args)
        if(v != null && v != newv)
          throw new Exception("Test failed: a node has nonequal outgoing hyperedges")
        else if(v == null) {
          runCache += (n,args) -> newv
          v = newv
        }
      } catch {
        case e: NonTerminationException =>
          // We don't crash here, vacuous path are ok if there are non-vacuous ones
          // just test this hyperedge later
          ctx.failed.add((h, args))
      }
    
    ctx.visited.remove((n, args))
      
    // None of the hyperedges has terminated, rollback
    if(v == null)
      throw new NonTerminationException("None of the hyperedges has terminated")
      
    v
  }
  
  def runHyperedgeUncached(ctx: RunningContext, h: Hyperedge, args: Vector[Value]): Value = {
    val res = h.run(args, this.runNode(ctx, _, _))
    ctx.failed.remove((h, args))
    res
  }
  
  def checkFailed(ctx: RunningContext) {
    ctx.visited.clear()
    if(ctx.failed.nonEmpty) {
      for((h, a) <- ctx.failed) {
        assert(runNode(ctx, h.source, a) == runHyperedgeUncached(ctx, h, a))
      }
      checkFailed(ctx)
    }
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    val ctx = new RunningContext
    for(((n, a), r) <- runCache if h.source == n) {
      assert(runHyperedgeUncached(ctx, h, a) == r)
    }
    checkFailed(ctx)
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(l: Node, r: Node) {
    val ctx = new RunningContext
    val data = for(((n, a), _) <- runCache if n == l || n == r) yield (n, a)
    for((n,a) <- data) {
      //println("testing " + a + " expect " + runCache((n,a)))
      assert(runNode(ctx, l, a) == runNode(ctx, r, a))
    }
    checkFailed(ctx)
    super.beforeGlue(l, r)
  }
  
  override def nodeDotLabel(n: Node): String = {
    n.uniqueName + "\\l" + 
    (for(((n1,a),r) <- runCache if n1 == n) yield
        a.mkString(", ") + " -> " + r).mkString("\\l") + "\\l"
  }
  
  def statistics() {
    val nodes = allNodes
    var empty = 0
    val nv =
      for(n <- nodes) yield
        (n, runCache.filter(_._1._1 == n).map(x => (x._1._2, x._2)).toMap)
    
    println(
        "statistics: " + nodes.size + 
        " should be " + fun(Set(), nv.toList).size + 
        " empty: " + empty)
    
    def fun(s: Set[(Node, Map[Vector[Value], Value])], 
            l: List[(Node, Map[Vector[Value], Value])]): Set[(Node, Map[Vector[Value], Value])] = {
      l match {
        case (n1, m1) :: tl =>
          if(m1.isEmpty)
            empty += 1
          else
            for((n, m) <- s) {
              if(m == m1)// || (m.toSet & m1.toSet).size >= 2) //m == m1
                return fun(s, tl)
            }
          fun(s + (n1 -> m1), tl)
        case Nil => s
      }
    }
  }
}
