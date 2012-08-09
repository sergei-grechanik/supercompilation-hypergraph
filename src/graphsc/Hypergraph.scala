package graphsc

trait Hypergraph {
  // h should be with known dests and null source
  // if source is not null then perform gluing
  def addHyperedge(h: Hyperedge)
    
  def addNode(n: Node): Node
  def addNode(l: Label, ds: List[Node]): Node = addNode(Node(l, ds))
  def newNode(used: Set[Int]): Node
  def removeNode(n: Node)
  
  // Nodes shouldn't be glued manually, they should be marked equal with 
  // a Renaming() hyperedge. Then the hypergraph should glue these nodes automatically.
  def glueNodes(l: Node, r: Node): Node
  
  def onNewHyperedge(h: Hyperedge) {}
  def beforeGlue(l: Node, r: Node) {}
  def afterGlue(l: Node) {}
  
  // Transformations should call this function before performing transformations
  // they should pass a list of hyperedges being transformed
  def transforming(h: Hyperedge*) {}
}

class TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    val n = Node(h)
    nodes += n
    val res = h.from(n)
    res.dests.foreach(_.insMut.add(res))
    res
  }
  
  def addHyperedgeImpl(h1: Hyperedge): Hyperedge = {
    val Hyperedge(l1, s1, dst1) = h1
    val h = Hyperedge(l1, s1, dst1.map(addNode(_))).derefGlued
    
    if(h.dests.nonEmpty)
      h.dests(0).ins.find(x => x.label == h.label && x.dests == h.dests) match {
        case Some(x) if h.source == null => x
        case Some(x) if h.source == x.source => x
        case Some(x) => glueNodes(h.source, x.source); x.derefGlued
        case None => 
          val newh = addHyperedgeSimple(h)
          onNewHyperedge(newh)
          newh
      }
    else
      nodes.find(_.outs.exists(_.label == h.label)) match {
        case Some(n) if h.source == null => h.from(n)
        case Some(n) => glueNodes(h.source, n); h.derefGlued
        case None => 
          val newh = addHyperedgeSimple(h)
          onNewHyperedge(newh)
          newh
      }
  }
  
  override def addHyperedge(h: Hyperedge) = {
    val Hyperedge(l, src, ds) = h  
    l match {
      case r: Renaming =>
        val r1 = r.reduce(ds(0).used)
        if(r1.isId)
          glueNodes(src, ds(0))
        else {
          // We also add a backward renaming
          addHyperedgeImpl(Hyperedge(r1.inv, ds(0), List(src)))
          addHyperedgeImpl(Hyperedge(r1, src, ds))
        }
      case Let(vars) if vars.isEmpty =>
        glueNodes(src, ds(0))
      case _ =>
        addHyperedgeImpl(Hyperedge(l, src, ds))
    }
  }
  
  override def addNode(n: Node): Node = {
    if(!nodes(n.getRealNode)) {
      nodes.add(n.getRealNode)
      for(h <- n.outs)
        addHyperedge(h)
    }
    n.getRealNode
  }
  
  override def newNode(used: Set[Int]): Node = {
    val n = new Node(used)
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
  }
  
  override def glueNodes(l1: Node, r1: Node): Node = {
    // just for convenience, this feature is undocumented, don't use it
    if(l1 == null) return r1
    if(r1 == null) return l1
    
    if(!nodes.contains(l1))
      addNode(l1)
    
    val l = l1.getRealNode
    val r = r1.getRealNode
    
    if(l != r) {
      beforeGlue(l, r)
      removeNode(r)
      l.mused = l.used & r.used
      r.gluedTo = l
      assert(nodes.forall(_.gluedTo == null))
      for(h <- r.mouts)
        addHyperedgeSimple(h.derefGlued)
      for(h <- r.mins)
        addHyperedgeSimple(h.derefGlued)
      afterGlue(l)
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
  
  def toDot: String = {
    val sb = new StringBuilder()
    sb.append("digraph Hyper {\n")
    for(n <- nodes) {
      sb.append("\"" + n.uniqueName + "\"[label=\"" + n.uniqueName + "\"];\n")
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
    for(n <- nodes) {
      for(h <- n.ins) {
        assert(h.source.outs(h))
        assert(h.dests.forall(_.ins(h)))
      }
      for(h <- n.outs) {
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
      val node = newNode((0 until arity).toSet)
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
        println(n.uniqueName + "(" + args + ") = " + v)
        v
      case None => 
        val res = runNodeUncached(ctx, n, args)
        println(n.uniqueName + "(" + args + ") = " + res)
        res
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
    val used = l.used & r.used
    for((n,a) <- data) {
      val newa =
        if(used.nonEmpty)
          Vector() ++
          (0 to used.max).map { i =>
            if(used(i)) a(i) else null
          }
        else
          Vector()
      
      if(newa != a)
        runCache.remove((n,a))
        
      assert(runNode(ctx, l, newa) == runNode(ctx, r, newa))
    }
    checkFailed(ctx)
    super.beforeGlue(l, r)
  }
  
  def statistics() {
    val nv =
      for(n <- nodes) yield
        (n, runCache.filter(_._1._1 == n).map(x => (x._1._2, x._2)).toMap)
    println("statistics: " + nodes.size + " should be " + fun(Set(), nv.toList).size)
    
    def fun(s: Set[(Node, Map[Vector[Value], Value])], 
            l: List[(Node, Map[Vector[Value], Value])]): Set[(Node, Map[Vector[Value], Value])] = {
      l match {
        case (n1, m1) :: tl =>
          for((n, m) <- s) {
            if(m == m1)
              return fun(s, tl)
          }
          fun(s + (n1 -> m1), tl)
        case Nil => s
      }
    }
  }
}
