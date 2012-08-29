package graphsc

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
  
  def runNode(ctx: RunningContext, n: Node, args: Vector[Value]): Value = {
    require(n.getRealNode == n)
    runCache.get((n,args)) match {
      case Some(v) =>
        //println(n.uniqueName + "(" + args + ") = " + v)
        v
      case None => 
        val res = runNodeUncached(ctx, n, args)
        //println(n.uniqueName + "(" + args + ") = " + res)
        res
    }
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
    val data = for(((n, a), _) <- runCache.toList if n == l || n == r) yield (n, a)
    for((n,a) <- data) {
      assert(runNode(ctx, l, a) == runNode(ctx, r, a))
    }
    checkFailed(ctx)
    super.beforeGlue(l, r)
  }
  
  override def nodeDotLabel(n: Node): String = {
    "\\l" + 
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
          for((n, m) <- s) {
            if(m == m1 || (m.toSet & m1.toSet).size >= 2) //m == m1
              return fun(s, tl)
          }
          fun(s + (n1 -> m1), tl)
        case Nil => s
      }
    }
  }
}
