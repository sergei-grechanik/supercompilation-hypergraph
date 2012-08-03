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

trait HyperTester extends Hypergraph {
  val runCache = collection.mutable.Map[(Node, Vector[Value]), Value]()
  
  def runNode(n: Node, args: Vector[Value]): Value = runCache.get((n,args)) match {
    case Some(null) => throw new NonTerminationException("Nontermination detected")
    case Some(v) => v
    case None => runNodeUncached(n, args)
  }
    
  def runNodeUncached(n: Node, args: Vector[Value]): Value = {
    runCache += (n,args) -> null
    var v: Value = null
    for(h <- n.outs)
      try {
        val newv = runHyperedgeUncached(h, args)
        if(v != null && v != newv)
          throw new Exception("Test failed: a node has nonequal outgoing hyperedges")
        else if(v == null) {
          runCache += (n,args) -> newv
          v = newv
        }
      } catch {
        case e: NonTerminationException =>
          // We don't crash here, vacuous path are ok if there are non-vacuous ones
      }
    
    if(v == null)
      throw new Exception("All hyperedges failed to produce anything")
      
    // Make sure that vacuous hyperedges don't crash anymore 
    for(h <- n.outs)
      assert(v == runHyperedgeUncached(h, args))
      
    v
  }
  
  def runHyperedgeUncached(h: Hyperedge, args: Vector[Value]): Value =
    h.run(args, this.runNode _)
    
  override def onNewHyperedge(h: Hyperedge) {
    for(((n, a), r) <- runCache if h.source == n) {
      assert(runHyperedgeUncached(h, a) == r)
    }
  }
  
  override def onGlue(l: Node, r: Node) {
    val data = for(((n, a), _) <- runCache if n == l || n == r) yield (n, a)
    val used = l.used & r.used
    for((n,a) <- data) {
      val newa = Vector() ++
        (0 to used.max).map { i =>
          if(used(i)) a(i) else null
        }
      
      if(newa != a)
        runCache.remove((n,a))
        
      assert(runNode(l, newa) == runNode(r, newa))
    }
  }
}
