package graphsc

trait Hypergraph {
  // h should be with known dests and null source
  // if source is not null then perform gluing
  def addHyperedge(h: Hyperedge): Hyperedge
  
  //def addHyperedgeSimple(h: Hyperedge): Hyperedge
  def addNode(used: Set[Int]): Node
  def removeNode(n: Node)
  def glueNodes(l: Node, r: Node): Node
  
  def onNewHyperedge(h: Hyperedge) {}
  def onNewNode(n: Node) {}
  // this function should be called before gluing
  def onGlue(l: Node, r: Node) {}
}

class TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    if(h.source == null) {
      val n = new Node(h.used)
      nodes += n
      val res = h.from(n)
      n.outs += res
      h.dests.foreach(_.ins.add(res))
      res
    }
    else {
      nodes += h.source
      h.source.outs += h
      h.dests.foreach(_.ins.add(h))
      h
    }
  }
  
  override def addHyperedge(h1: Hyperedge): Hyperedge = {
    val h = h1.derefGlued
    if(h.dests.nonEmpty)
      h.dests(0).ins.find(x => x.label == h.label && x.dests == h.dests) match {
        case Some(x) if h.source == null => x
        case Some(x) if h.source == x.source => x
        case Some(x) => glueNodes(h.source, x.source); h
        case None => 
          val newh = addHyperedgeSimple(h)
          onNewHyperedge(newh)
          newh
      }
    else
      nodes.find(_.outs.exists(_.label == h.label)) match {
        case Some(n) if h.source == null => h.from(n)
        case Some(n) => glueNodes(h.source, n); h
        case None => 
          val newh = addHyperedgeSimple(h)
          onNewHyperedge(newh)
          newh
      }
  }
  
  override def addNode(used: Set[Int]): Node = {
    val n = new Node(used)
    nodes.add(n)
    onNewNode(n)
    n
  }
  
  override def removeNode(n: Node) {
    nodes -= n
    // we should leave n.outs and n.ins intact
    for(h <- n.ins if h.source != n)
      h.source.outs -= h
    for(h <- n.outs; d <- h.dests if d != n)
      d.ins -= h
  }
  
  override def glueNodes(l1: Node, r1: Node): Node = {
    val l = l1.getRealNode
    val r = r1.getRealNode
    require(nodes.contains(l) && nodes.contains(r))
    
    if(l != r) {
      onGlue(l, r)
      removeNode(r)
      r.gluedTo = l
      l.mused = l.used & r.used
      for(h <- r.mouts)
        addHyperedgeSimple(h.replace(r, l))
      for(h <- r.mins)
        addHyperedgeSimple(h.replace(r, l))
      // maybe there appeared some more nodes to glue 
      afterGlue(l)
      // Now l may be glued to something else
      l.getRealNode
    }
    else //if(l == r)
      l // Nodes are already glued
  }
  
  // glue parents recursively
  def afterGlue(n: Node) {
    val groups = n.ins.groupBy(h => (h.label, h.dests)).filter(_._2.size > 1)
    for((_, g) <- groups)
      g.toList.map(_.source).reduce(glueNodes)
  }
  
  def toDot: String = {
    val sb = new StringBuilder()
    sb.append("digraph Hyper {\n")
    for(n <- nodes) {
      for(h <- n.outs) {
        sb.append("\"" + h.toString + "\"[label=\"" + h.label.toString + "\"];\n")
        sb.append("\"" + n.toString + "\" -> \"" + h.toString + "\";\n")
        for(d <- h.dests)
          sb.append("\"" + h.toString + "\" -> \"" + d.toString + "\";\n")
      }
      sb.append("\n")
    }
    sb.append("}\n")
    sb.toString
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
      val node = addNode((0 until arity).toSet)
      namedNodes += n -> node
      node
    }
}

class NonTerminationException(s: String) extends Exception(s)

class RunningContext {
  val visited = collection.mutable.Set[(Node, Vector[Value])]()
  val failed = collection.mutable.Set[(Hyperedge, Vector[Value])]()
}

trait HyperTester extends Hypergraph {
  val runCache = collection.mutable.Map[(Node, Vector[Value]), Value]()
  
  def runNode(n: Node, args: Vector[Value]): Value = {
    val ctx = new RunningContext
    val res = runNode(new RunningContext, n, args)
    checkFailed(ctx)
    res
  }
  
  def runNode(ctx: RunningContext, n: Node, args: Vector[Value]): Value = 
    runCache.get((n,args)) match {
      case Some(v) => v
      case None => runNodeUncached(ctx, n, args)
    }
    
  def runNodeUncached(ctx: RunningContext, n: Node, args: Vector[Value]): Value = {
    if(ctx.visited((n,args)))
      throw new NonTerminationException("Nontermination detected")
    
    ctx.visited.add((n,args))
    var v: Value = null
    for(h <- n.outs)
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
    println("new " + h)
    val ctx = new RunningContext
    for(((n, a), r) <- runCache if h.source == n) {
      assert(runHyperedgeUncached(ctx, h, a) == r)
    }
    checkFailed(ctx)
    super.onNewHyperedge(h)
  }
  
  override def onGlue(l: Node, r: Node) {
    val ctx = new RunningContext
    val data = for(((n, a), _) <- runCache if n == l || n == r) yield (n, a)
    val used = l.used & r.used
    for((n,a) <- data) {
      val newa = Vector() ++
        (0 to used.max).map { i =>
          if(used(i)) a(i) else null
        }
      
      if(newa != a)
        runCache.remove((n,a))
        
      assert(runNode(ctx, l, newa) == runNode(ctx, r, newa))
    }
    checkFailed(ctx)
    super.onGlue(l, r)
  }
}
