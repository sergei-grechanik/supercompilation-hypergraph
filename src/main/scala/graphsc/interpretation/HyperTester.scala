package graphsc
package interpretation

class NonTerminationException(s: String = "") extends Exception(s)

case class RunningContext(limit: Int = Int.MaxValue, visited: List[(Node, List[Value])] = List()) {
  def add(n: Node, a: List[Value]): RunningContext =
    RunningContext(limit, (n, a)::visited)
  def -(c: Int): RunningContext =
    RunningContext(limit - c, visited)
}

trait HyperTester extends TheHypergraph {
  def onTheFlyTesting = false
  
  val runCacheImpl = 
    collection.mutable.Map[Node, collection.mutable.Map[List[Value], ValueAndStuff]]()
  
  def runCache(n: Node): collection.mutable.Map[List[Value], ValueAndStuff] =
    runCacheImpl.getOrElseUpdate(n, collection.mutable.Map())
  
  def clearRunCache() {
    runCacheImpl.clear()
  }
  
  def runNode(n: RenamedNode, args: List[Value]): ValueAndStuff = {
    val ctx = RunningContext()
    val res = runNode(ctx, n, args)
    res
  }
  
  def runNode(ctx: RunningContext, node: RenamedNode, argsUncut: List[Value]): ValueAndStuff = {
    val n = node.deref.node
    val args =
      truncArgs(node.deref, argsUncut).map(_ | Bottom)
      
    val almost = runCache(n).get(args) match {
      case Some(v) => v
      case None => runNodeUncached(ctx, n, args)
    }
    
    // If the result contains bottom then it may have
    // been taken from the arguments, so if they contain ErrorBottom, we should propagate it
    if(!argsUncut.contains(ErrorBottom) || almost.value.isBottomless)
      almost
    else
      ValueAndStuff(ErrorBottom, 0, Nil) 
  }
    
  // args should be without ErrorBottoms
  def runNodeUncached(ctx: RunningContext, n: Node, args: List[Value]): ValueAndStuff = {
    require(n.outs.nonEmpty)
    require(args.forall(_ != ErrorBottom))
    
    if(ctx.visited.contains((n,args)) || ctx.limit <= 0)
      return ValueAndStuff(ErrorBottom, 0, Nil)

    var newctx = ctx.add(n,args)

    // Try id hyperedges first
    val outs = 
      n.outs.toList.sortBy(h => h.label match {
        case _: Id => 0
        case _ if isDefining(h) => 1
        case _ => 2
      })
    
    val values = 
      for(o <- outs) yield {
        val r = runHyperedgeUncached(newctx, o, args)
        
        // the answer may have been computed deeper
        runCache(n).get(args) match {
          case Some(x) =>
            return x
          case _ =>
        }
        
        // Next hyperedges mustn't take much longer to compute
        if(r.value != ErrorBottom) {
          val newlim = limitFromMinCost(r.cost)
          if(newlim < newctx.limit)
            newctx = RunningContext(newlim, newctx.visited)
        }
          
        r
      }
    
    val lub = values.reduce(_ | _)
    
    if(lub.value != ErrorBottom) {
      runCache(n) += args -> lub
      
      for((v,o) <- values zip outs if v != lub) {
        val test = runHyperedgeUncached(RunningContext(newctx.limit), o, args)
        // Sometimes it is just too difficult to compute the true value
        if(test.value != ErrorBottom)
          assert(test.value == lub.value)
        else {
          // If we have a hyperedge like this:
          //   f x y = f x (S y)
          // we will get an infinite branch and hence this error message.
          // Scince there is an example showing this problem (samples/dummy),
          // I've disabled this error message.
          //println("Warning: there was an error computing\n\t" + o + " " + args)
          //println("Prettified source:")
          //println(n.prettyDebug)
        }
      }
    }
      
    lub
  }
  
  // Note that this function runs arguments through h.source.renaming.inv
  private def runHyperedgeUncached(
      ctx: RunningContext, h: Hyperedge, argsUncut: List[Value]): ValueAndStuff = {
    val args = 
      truncArgs(h.source.renaming.inv comp h.asDummyNode, argsUncut)
      
    var curctx = ctx
    var subcost = 0
      
    val rv = runHyperedge(h, args, { (n, as) =>
      val ValueAndStuff(v, c, _) = this.runNode(curctx, n, as)
      curctx -= c
      subcost += c
      v
    })
    
    ValueAndStuff(rv, subcost + hyperedgeCost(h), List(h))
  }
  
  // Pure hyperedge cost
  def hyperedgeCost(h: Hyperedge): Int = h.label match {
    case Construct(name) => 1
    case CaseOf(cases) => 1
    case Let() => 1
    case Tick() => 1
    case Improvement() => 1
    case Id() => 1
    case Var() => 1
    case Unused() => 0
  }
  
  def updateRunCache(n: Node, as: List[Value], r: ValueAndStuff) {
    val cache = runCache(n)
    cache.get(as) match {
      case None => cache += as -> r
      case Some(oldr) =>
        cache += as -> (r | oldr)
        if(r.cost < oldr.cost)
          onMinCostChanged(n)
    }
  }
  
  // If the right costs are really needed on the fly, this function should be overridden
  // to retest incoming hyperedges. The obvious method of retesting is too slow, so
  // it is disabled by default. It's easier to run tests from scratch before residualization.
  def onMinCostChanged(n: Node) {
    //for(h <- n.insMut)
    //  retestHyperedge(h)
  }
  
  def limitFromMinCost(c: Int): Int = c*2 + 30
  
  def retestHyperedge(h: Hyperedge) {
    for((as, r) <- runCache(h.source.node)) {
      val ctx = RunningContext(limitFromMinCost(r.cost))
      val res = runHyperedgeUncached(ctx, h, as)
      
      if(res.value != r.value) {
        System.err.println("Hyperedge test failed")
        System.err.println("args = " + as)
        System.err.println("Got " + res.value + "  should be " + r.value)
        this match {
          case pret: Prettifier =>
            System.err.println("\nNode: \n" + pret.pretty(h.source.node) + "\n")
            System.err.println("\nHyperedge: \n" + h +"\n\n" + pret.prettyHyperedge(h) + "\n")
          case _ =>
        }
        runHyperedgeUncached(RunningContext(), h, as)
        throw new Exception("Hyperedge test failed")
      }
      
      updateRunCache(h.source.node, as, res)
    }
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    if(onTheFlyTesting)
      retestHyperedge(h)
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    if(onTheFlyTesting) {
      val ctx = RunningContext()
      val renamed_r = RenamedNode(l.renaming.inv, r).normal
      val data = 
        runCache(l.node).toList.map(p => truncArgs(renamed_r, p._1)) ++
        runCache(r).toList.map(_._1)
      for(as <- data) {
        val lres = runNode(ctx, l, as)
        val rres = runNode(ctx, RenamedNode.fromNode(r), as)
        
        if(lres.value != rres.value) {
          System.err.println("Node merging test failed")
          System.err.println("args = " + as)
          System.err.println("Left result = " + lres.value)
          System.err.println("Right result = " + rres.value)
          this match {
            case pret: Prettifier =>
              System.err.println("\nLeft: \n" + pret.pretty(l) + "\n")
              System.err.println("\nRight: \n" + pret.pretty(r) + "\n")
            case _ =>
          }
          runNode(ctx, l, as)
          runNode(ctx, RenamedNode.fromNode(r), as)
          throw new Exception("Node merging test failed")
        }
        
        updateRunCache(l.node, truncArgs(l, as), lres | rres)
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
        val res = runNode(node, as)
        
        if(res.value != r.value) {
          System.err.println("Used reduction test failed")
          System.err.println("args = " + as)
          System.err.println("Got " + res + "  should be " + r)
          this match {
            case pret: Prettifier =>
              System.err.println("\nNode: \n" + pret.pretty(n) + "\n")
            case _ =>
          }
          runNode(node, as)
          throw new Exception("Used reduction test failed")
        }
        
        updateRunCache(n, as, res)
      }
    }
    super.onUsedReduced(n)
  }
  
  private def truncArgs(n: RenamedNode, as: List[Value]): List[Value] = {
    val norm = n.normal
    norm.renaming.vector.map(i => if(i >= 0 && i < as.size) as(i) else Bottom)
  }
}

trait OnTheFlyTesting extends HyperTester {
  override def onTheFlyTesting = true
}
