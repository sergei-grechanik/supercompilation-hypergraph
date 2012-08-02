package graphsc

trait Hypergraph {
  // h should be with known dests and null source
  // if source is not null then perform gluing
  def addHyperedge(h: Hyperedge): Hyperedge
  
  def addHyperedgeSimple(h: Hyperedge): Hyperedge
  def addNode(arity: Int): Node
  def removeNode(n: Node)
  def glueNodes(l: Node, r: Node): Node
  
  def onNewHyperedge(h: Hyperedge) {}
  def onNewNode(n: Node) {}
  // this function should be called before gluing
  def onGlue(l: Node, r: Node) {}
}

class TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  override def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    if(h.source == null) {
      val n = new Node(h.arity)
      nodes += n
      val res = h.from(n)
      n.outs += res
      h.dests.foreach(_.ins.add(res))
      onNewHyperedge(res)
      res
    }
    else {
      nodes += h.source
      h.source.outs += h
      h.dests.foreach(_.ins.add(h))
      onNewHyperedge(h)
      h
    }
  }
  
  override def addHyperedge(h: Hyperedge): Hyperedge = {
    // If this exception happens, try to perform actual gluing 
    // when no one works with the hypergraph
    if((h.source != null && h.source.gluedTo != null) || h.dests.exists(_.gluedTo != null))
      throw new Exception("Adding a hyperedge with glued nodes, may be a bug")
    
    if(h.dests.nonEmpty)
      h.dests(0).ins.find(x => x.label == h.label && x.dests == h.dests) match {
        case Some(x) if h.source == null => x
        case Some(x) if h.source == x.source => x
        case Some(x) => glueNodes(h.source, x.source); h
        case None => addHyperedgeSimple(h)
      }
    else
      nodes.find(_.outs.exists(_.label == h.label)) match {
        case Some(n) if h.source == null => h.from(n)
        case Some(n) => glueNodes(h.source, n); h
        case None => addHyperedgeSimple(h)
      }
  }
  
  override def addNode(arity: Int): Node = {
    val n = new Node(arity)
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
    if(nodes.contains(l) && nodes.contains(r) && l != r) {
      onGlue(l, r)
      removeNode(r)
      r.gluedTo = l
      for(h <- r.outs)
        addHyperedgeSimple(h.replace(r, l))
      for(h <- r.ins)
        addHyperedgeSimple(h.replace(r, l))
      // maybe there appeared some more nodes to glue 
      afterGlue(l)
      // Now l may be glued to something else
      l.getRealNode
    }
    else if(l == r)
      l // Nodes are already glued
    else
      // Well, you shouldn't do this, they don't belong to this graph
      throw new IllegalArgumentException("Cannot glue nodes which aren't in this graph")
      // But maybe we should return null here, I'm not sure
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
        sb.append("node \"" + h.toString + "\"[label=\"" + h.label.toString + "\"];\n")
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
  
  def addNode(n: String, a: Int): Node = 
    if(namedNodes.contains(n)) {
      namedNodes(n)
    }
    else {
      val node = addNode(a)
      namedNodes += n -> node
      node
    }
}


trait HyperTester extends Hypergraph {
  val runCache = collection.mutable.Map[(Node, List[Value]), Value]()
  
  def runNode(n: Node, args: List[Value]): Value = 
    runCache.getOrElseUpdate((n, args), runNodeUncached(n, args))
  
  def runNodeUncached(n: Node, args: List[Value]): Value = {
    var v: Value = null
    for(h <- n.outs) {
      val newv = runHyperedgeUncached(h, args)
      if(v != null && v != newv)
        throw new Exception("Test failed: a node has nonequal outgoing hyperedges")
      else if(v == null)
        v = newv
    }
    v
  }
  
  def runHyperedgeUncached(h: Hyperedge, args: List[Value]): Value =
    h.run(args, this.runNode _)
    
  override def onNewHyperedge(h: Hyperedge) {
    for(((n, a), r) <- runCache if h.source == n) {
      assert(runHyperedgeUncached(h, a) == r)
    }
  }
  
  override def onGlue(l: Node, r: Node) {
    val data = for(((n, a), _) <- runCache if n == l || n == r) yield a
    for(a <- data)
      assert(runNode(l,a) == runNode(r,a))
  }
}
