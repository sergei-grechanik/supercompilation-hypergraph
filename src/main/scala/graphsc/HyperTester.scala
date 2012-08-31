package graphsc

class NonTerminationException(s: String = "") extends Exception(s)

case class RunningContext(depth: Int = 0, visited: List[(Node, Vector[Value])] = List()) {
  def add(n: Node, a: Vector[Value]): RunningContext =
    RunningContext(depth + 1, (n, a)::visited)
}

trait HyperTester extends TheHypergraph {
  val runCacheImpl = collection.mutable.Map[Node, collection.mutable.Map[Vector[Value], Value]]()
  
  def runCache(n: Node): collection.mutable.Map[Vector[Value], Value] =
    runCacheImpl.getOrElseUpdate(n, collection.mutable.Map())
    
  def depthLimit = 50
  
  def runNode(n: Node, args: Vector[Value]): Value = {
    val ctx = RunningContext()
    val res = runNode(ctx, n, args)
    res
  }
  
  def runNode(ctx: RunningContext, n: Node, args: Vector[Value]): Value = {
    require(n.getRealNode == n)
    val args1 = args.map(_ | Bottom)
    val almost = runCache(n).get(args1) match {
      case Some(v) => v
      case None => runNodeUncached(ctx, n, args1)
    }
    
    // If the result contains bottom then it may have
    // been taken from the arguments, so if they contain ErrorBottom, we should propagate it
    if(!args.contains(ErrorBottom) || almost.isBottomless)
      almost
    else
      ErrorBottom
  }
    
  // args should be without ErrorBottoms
  def runNodeUncached(ctx: RunningContext, n: Node, args: Vector[Value]): Value = {
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
      assert(!args.forall(_.isBottomless) || lub.isBottomless)
      
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
  
  def runHyperedgeUncached(ctx: RunningContext, h: Hyperedge, args: Vector[Value]): Value = {
    val res = h.run(args, this.runNode(ctx, _, _))
    res
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    val ctx = RunningContext()
    for((a, r) <- runCache(h.source)) {
      assert(runHyperedgeUncached(ctx, h, a) == r)
    }
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(l: Node, r: Node) {
    val ctx = RunningContext()
    val data = for(n <- List(l,r); (a, _) <- runCache(n).toList) yield (n, a)
    for((n,a) <- data) {
      assert(runNode(ctx, l, a) == runNode(ctx, r, a))
    }
    super.beforeGlue(l, r)
  }
  
  override def nodeDotLabel(n: Node): String = {
    "\\l" + 
    (for((a,r) <- runCache(n)) yield
        a.mkString(", ") + " -> " + r).mkString("\\l") + "\\l"
  }
  
  def statistics() {
    val nodes = allNodes
    var empty = 0
    val nv =
      for(n <- nodes) yield
        (n, runCache(n).toMap)
    
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

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

sealed trait Value {
  def size: Int
  def |(v: Value): Value
  def isBottomless: Boolean
}

case class Ctr(constructor: String, args: List[Value]) extends Value {
  override def toString = constructor + " " + args.map("(" + _ + ")").mkString(" ")
  override def size = 1 + args.map(_.size).sum
  
  def |(v: Value): Value = v match {
    case Ctr(c1, a1) if c1 == constructor && a1.length == args.length =>
      Ctr(c1, args zip a1 map { case (l,r) => l | r })
    case Bottom =>
      this
    case ErrorBottom =>
      this
    case _ =>
      throw new Exception("Values are incompatible")
  }
  
  override def isBottomless = args.forall(_.isBottomless)
}

case object Bottom extends Value {
  override def toString = "_|_"
  override def size = 1
  def |(v: Value): Value = v match {
    case ErrorBottom => Bottom
    case v => v
  }
  override def isBottomless = false
}

case object ErrorBottom extends Value {
  override def toString = "_[fail]_"
  override def size = 1
  def |(v: Value): Value = v
  
  override def isBottomless = false
}
