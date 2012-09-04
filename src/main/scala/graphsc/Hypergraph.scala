package graphsc

trait Hypergraph {
  // h should connect nodes known to the hypergraph
  // h.source may be a free node, in this case the function should add or find an appropriate node 
  // This function may perform transformations
  def addHyperedge(h: Hyperedge): Node
  
  // add a hyperedge, shortcut
  def add(l: Label, src: Node, ds: List[Node]): Node =
    addHyperedge(Hyperedge(l, src, ds))
  
  // Create a node with a hyperedge (or return an existing one)
  def add(l: Label, ds: List[Node]): Node =
    addHyperedge(Hyperedge(l, null, ds).freeSource)
  
  // Create a node without any connections
  def newNode(arity: Int): Node
  
  // Nodes shouldn't be glued manually, they should be marked equal with 
  // an Id() hyperedge. Then the hypergraph should glue these nodes automatically.
  // def glueNodes(l: Node, r: Node): Node
  
  def onNewHyperedge(h: Hyperedge) {}
  def beforeGlue(l: Node, r: Node) {}
  def afterGlue(l: Node) {}
  
  // deprecated
  def allNodes: Set[Node] =
    null
}

trait TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  override def allNodes: Set[Node] = nodes.toSet
  
  def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    val res =
      if(h.source.isInstanceOf[FreeNode]) {
        val n = newNode(h.arity)
        h.source.gluedTo = n
        h.from(n)
      }
      else
        h
    
    res.source.outsMut += res
    res.dests.foreach(_.insMut.add(res))
    res
  }
  
  def addHyperedgeImpl(h1: Hyperedge): Node = {
    val h = h1.derefGlued
    
    require(h.source.isInstanceOf[FreeNode] || nodes(h.source))
    
    if(h.dests.nonEmpty)
      h.dests(0).ins.find(x => x.label == h.label && x.dests == h.dests) match {
        case Some(x) if h.source == x.source => x.source
        case Some(x) => glueNodes(x.source, h.source)
        case None => 
          val newh = addHyperedgeSimple(h)
          checkIntegrity()
          onNewHyperedge(newh)
          newh.source
      }
    else
      nodes.find(_.outs.exists(_.label == h.label)) match {
        case Some(n) => glueNodes(n, h.source)
        case None => 
          val newh = addHyperedgeSimple(h)
          checkIntegrity()
          onNewHyperedge(newh)
          newh.source
      }
  }
  
  def addHyperedge(h: Hyperedge): Node = {
    val Hyperedge(l, src, ds) = h
    l match {
      case _ if h.isId =>
        glueNodes(ds(0), src)
      case Renaming(a, vec) if 
        a == vec.length && vec.zipWithIndex.forall{ case (a,b) => a == b } =>
        // we do both because renamings mark canonicalized nodes
        val n = glueNodes(ds(0), src)
        addHyperedgeImpl(Hyperedge(l, n, List(n)))
      case Renaming(a, vec) =>
        addHyperedgeImpl(h)
        // add the inverse renaming as well
        if(a == vec.length && vec.toSet == (0 until a).toSet) {
          val newvec = vec.zipWithIndex.sortBy(_._1).map(_._2)
          addHyperedgeImpl(Hyperedge(Renaming(a, newvec), ds(0), List(src)))
        }
        h.source.realNode
      case _ =>
        addHyperedgeImpl(h)
    }
  }
  
  override def newNode(arity: Int): Node = {
    val n = new Node(arity)
    nodes.add(n)
    n
  }
  
  def removeNode(n: Node) {
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
  
  def glueNodes(l1: Node, r1: Node): Node = {
    // just for convenience, this feature is undocumented, don't use it
    if(l1 == null) return r1.realNode
    if(r1 == null) return l1.realNode
    
    if(l1.isInstanceOf[FreeNode] && l1.gluedTo == null) {
      assert(nodes.contains(r1.realNode))
      l1.gluedTo = r1.realNode
      return l1.realNode
    }
    
    if(r1.isInstanceOf[FreeNode] && r1.gluedTo == null) {
      assert(nodes.contains(l1.realNode))
      r1.gluedTo = l1.realNode
      return r1.realNode
    }
    
    assert(nodes.contains(l1.realNode) && nodes.contains(r1.realNode))
    
    val l = l1.realNode
    val r = r1.realNode
    
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
      l.realNode
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
      assert(n.realNode == n)
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


