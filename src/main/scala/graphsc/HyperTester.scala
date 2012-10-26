package graphsc

class NonTerminationException(s: String = "") extends Exception(s)

case class RunningContext(depth: Int = 0, visited: List[(Node, List[Value])] = List()) {
  def add(n: Node, a: List[Value]): RunningContext =
    RunningContext(depth + 1, (n, a)::visited)
}

trait HyperTester extends TheHypergraph {
  def onTheFlyTesting = false
  
  val runCacheImpl = collection.mutable.Map[Node, collection.mutable.Map[List[Value], Value]]()
  
  def runCache(n: Node): collection.mutable.Map[List[Value], Value] =
    runCacheImpl.getOrElseUpdate(n, collection.mutable.Map())
    
  def depthLimit = 150
  
  def runNode(n: RenamedNode, args: List[Value]): Value = {
    val ctx = RunningContext()
    val res = runNode(ctx, n, args)
    res
  }
  
  def runNode(ctx: RunningContext, node: RenamedNode, argsUncut: List[Value]): Value = {
    val n = node.deref.node
    val args =
      truncArgs(node.deref, argsUncut).map(_ | Bottom)
      
    val almost = runCache(n).get(args) match {
      case Some(v) => v
      case None => runNodeUncached(ctx, n, args)
    }
    
    // If the result contains bottom then it may have
    // been taken from the arguments, so if they contain ErrorBottom, we should propagate it
    if(!argsUncut.contains(ErrorBottom) || almost.isBottomless)
      almost
    else
      ErrorBottom
  }
    
  // args should be without ErrorBottoms
  def runNodeUncached(ctx: RunningContext, n: Node, args: List[Value]): Value = {
    require(n.outs.nonEmpty)
    require(args.forall(_ != ErrorBottom))
    
    if(ctx.visited.contains((n,args)) || ctx.depth > depthLimit)
      return ErrorBottom

    val newctx = ctx.add(n,args)

    // Try id hyperedges first
    val outs = 
      n.outs.filter(_.label.isInstanceOf[Id]).toList ++
      n.outs.filter(!_.label.isInstanceOf[Id]).toList
      
    val values = 
      for(o <- outs) yield {
        val r = runHyperedgeUncached(newctx, o, args)
        
        // the answer may have been computed deeper
        runCache(n).get(args) match {
          case Some(x) =>
            return x
          case _ =>
        }
        
        r
      }
    
    val lub = values.reduce(_ | _)
    
    if(lub != ErrorBottom) {
      runCache(n) += args -> lub
      
      for((v,o) <- values zip outs if v != lub) {
        val test = runHyperedgeUncached(RunningContext(newctx.depth), o, args)
        // Sometimes it is just too difficult to compute the true value
        if(test != ErrorBottom)
          assert(test == lub)
        else {
          println("Warning: there was an error computing\n\t" + o + " " + args)
          println("Prettified source:")
          println(n.prettyDebug)
        }
      }
    }
      
    lub
  }
  
  // Note that this function runs arguments through h.source.renaming.inv
  private def runHyperedgeUncached(
      ctx: RunningContext, h: Hyperedge, argsUncut: List[Value]): Value = {
    val args = 
      truncArgs(h.source.renaming.inv comp h.asDummyNode, argsUncut)
        
    val res = h.run(args, this.runNode(ctx, _, _))
    res
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    if(onTheFlyTesting)
      for((a, r) <- runCache(h.source.node)) {
        val ctx = RunningContext()
        val res = runHyperedgeUncached(ctx, h, a)
        assert(res == r)
      }
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    if(onTheFlyTesting) {
      val ctx = RunningContext()
      val renamed_r = RenamedNode(l.renaming.inv, r).normal
      val data = 
        runCache(l.node).toList.map(p => truncArgs(renamed_r, p._1))
        runCache(r).toList.map(_._1)
      for(a <- data) {
        assert(runNode(ctx, l, a) == runNode(ctx, RenamedNode.fromNode(r), a))
      }
    }
    super.beforeGlue(l, r)
  }
  
  override def onUsedReduced(n: Node) {
    if(onTheFlyTesting) {
      val node = RenamedNode.fromNode(n)
      val cache = runCache(n)
      val data = 
        cache.toList.map {
          case (as,r) => 
            truncArgs(node, as) -> r
        }
          
      cache.clear()
      for((as,r) <- data) {
        val newr = runNode(node, as)
        assert(newr == r)
      }
    }
    super.onUsedReduced(n)
  }
  
  private def truncArgs(n: RenamedNode, as: List[Value]): List[Value] = {
    val norm = n.normal
    norm.renaming.vector.map(i => if(i >= 0 && i < as.size) as(i) else Bottom)
  }
  
  override def nodeDotLabel(n: Node): String = {
    super.nodeDotLabel(n) +
    "\\l" + 
    (for((a,r) <- runCache(n) /*if a.forall(_.isBottomless)*/) yield
        a.mkString(", ") + " -> " + r).mkString("\\l") + "\\l"
  }
  
  def statisticsTester() {
    val nodes = allNodes
    var empty = 0
    val nv =
      for(n <- nodes) yield
        (n, runCache(n).toMap)
        
    println(
        "statistics: " + nodes.size + 
        " should be " + (fun(Set(), nv.toList).size + empty) + 
        " empty: " + empty)
    
    def fun(s: Set[(Node, Map[List[Value], Value])], 
            l: List[(Node, Map[List[Value], Value])]): Set[(Node, Map[List[Value], Value])] = {
      l match {
        case (n1, m1) :: tl =>
          if(m1.isEmpty)
            empty += 1
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

trait OnTheFlyTesting extends HyperTester {
  override def onTheFlyTesting = true
}
