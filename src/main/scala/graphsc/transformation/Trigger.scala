package graphsc
package transformation

// TODO: Since arity may be reduced, destsize constraint might not be very reliable
trait Triggers extends Hypergraph with NamedNodes {
  val inTriggers = 
    collection.mutable.Map[Node, 
      collection.mutable.Map[(Label, Int, Int), collection.mutable.Stack[Hyperedge => Unit]]]()
  val outTriggers = 
    collection.mutable.Map[Node, 
      collection.mutable.Map[(Label, Int), collection.mutable.Stack[Hyperedge => Unit]]]()
  val glueTriggers =
    collection.mutable.Map[Node, 
      collection.mutable.Map[Node, collection.mutable.Stack[() => Unit]]]()
      
  def addInTrigger(node: Node, label: Label, destsize: Int, nodeindex: Int, f: Hyperedge => Unit) {
    log("-- in trigger: " + nodeToString(node.deref) + 
        " <- " + label + " " + destsize + " " + nodeindex)
    inTriggers.getOrElseUpdate(node, collection.mutable.Map())
      .getOrElseUpdate((label, destsize, nodeindex), collection.mutable.Stack()).push(f)
      
    for(h <- node.ins; if h.label == label && h.dests.size == destsize;
        if h.dests(nodeindex).node == node) {
      log("-- in trigger fired immediately")
      logShift()
      f(h)
      logUnshift()
    }
  }
  
  def addOutTrigger(node: Node, label: Label, destsize: Int, f: Hyperedge => Unit) {
    log("-- out trigger: " + nodeToString(node.deref) + " -> " + label + " " + destsize)
    outTriggers.getOrElseUpdate(node, collection.mutable.Map())
      .getOrElseUpdate((label, destsize), collection.mutable.Stack()).push(f)
      
    for(h <- node.outs; if h.label == label && h.dests.size == destsize) {
      log("-- out trigger fired immediately")
      logShift()
      f(h)
      logUnshift()
    }
  }
  
  // TODO: It is more efficient to assign the trigger to the less popular node
  // TODO: We should remove the trigger if we know for sure that the nodes are incompatible
  def addGlueTrigger(node: Node, node2: Node, f: () => Unit) {
    log("-- glue trigger: " + nodeToString(node.deref) + " ~ " + nodeToString(node2.deref))
    
    if(node ~~ node2) {
      log("-- glue trigger fired immediately")
      logShift()
      f()
      logUnshift()
    }
    else
      glueTriggers.getOrElseUpdate(node, collection.mutable.Map())
        .getOrElseUpdate(node2, collection.mutable.Stack()).push(f)
  }
  
  // TODO: remove triggers for defining hyperedges 
  def procHyperedge(h: Hyperedge) {
    val sz = h.dests.size
    for((d,i) <- h.dests.zipWithIndex; 
        map <- inTriggers.get(d.deref.node);
        fs <- map.get((h.label, sz, i));
        f <- fs) {
      log("-- in trigger fired: " + hyperedgeToString(h))
      f(h)
    }
    
    for(map <- outTriggers.get(h.source.deref.node); fs <- map.get((h.label, sz)); f <- fs) {
      log("-- out trigger fired: " + hyperedgeToString(h))
      f(h)
    }
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    procHyperedge(h)
    super.onNewHyperedge(h)
  }
  
  override def afterHyperedgeChanged(old: Hyperedge, cur: Hyperedge) {
    procHyperedge(cur)
    super.afterHyperedgeChanged(old, cur)
  }
  
  override def afterGlue(kept: Node, removed: Node) {
    assert(kept != removed)
    
    for(map <- inTriggers.get(removed)) {
      val keptmap = inTriggers.getOrElseUpdate(kept, collection.mutable.Map())
      for((k,s) <- map) {
        keptmap.get(k) match {
          case None => keptmap += k -> s
          case Some(keptstack) => keptstack.pushAll(s)
        }
      }
    }
    
    for(map <- outTriggers.get(removed)) {
      val keptmap = outTriggers.getOrElseUpdate(kept, collection.mutable.Map())
      for((k,s) <- map) {
        keptmap.get(k) match {
          case None => keptmap += k -> s
          case Some(keptstack) => keptstack.pushAll(s)
        }
      }
    }
    
    {
      val keptmap = glueTriggers.getOrElseUpdate(kept, collection.mutable.Map())
      
      for(map <- glueTriggers.get(removed); (n,fs) <- map) {
        keptmap.getOrElseUpdate(n, collection.mutable.Stack()).pushAll(fs)
      }
      
      glueTriggers -= removed
      
      val ns = for((n,fs) <- keptmap if n ~~ kept) yield {
        log("-- glue trigger fired: " + nodeToString(kept.deref) + " ~ " + 
            removed.prettyDebug.replace("\n", " "))
        logShift()
        fs.foreach(_())
        logUnshift()
        n
      }
      
      keptmap --= ns
    }
    
    super.afterGlue(kept, removed)
  }
  
  def addPatternTrigger(expr: Expr, f: (RenamedNode, Subst) => Unit) {
    if(!addPatternTriggerMB(expr, f))
      throw new Exception("Unsupported construction in a pattern:\n" + expr)
  }
  
  def addPatternTriggerMB(expr: Expr, f: (RenamedNode, Subst) => Unit): Boolean = {
    log("-- addPatternTriggerMB " + expr)
    expr match {
      case ExprCall(ExprFun(name), as) =>
        val n = newNode(name, as.size)
        addInTrigger(n.deref.node, Let(), as.size + 1, 0, fun)
        def fun(h: Hyperedge) {
          // h.dests(0) = ren comp n
          val ren = h.dests(0).renaming comp n.deref.renaming.inv
          val pairs = ren.vector.map(i => h.dests(i + 1)) zip as
          
          addOutPatternTriggers(pairs, fun1)
          def fun1(lst: List[RenamedNode], subst: Subst) {
            // h.source = name(as(subst))
            log("-- " + nodeToString(h.source) + " matches " + expr)
            logSubst(this, subst)
            f(h.source, subst)
          }
        }
        
        if(as.forall(_.isInstanceOf[ExprVar]) && as.distinct.size == as.size) {
          f(n, as.zipWithIndex.map { 
            case (ExprVar(v), i) => v -> variable(i)
            case _ => throw new Exception("Impossible")
          }.toMap)
        }
        
        true
      case ExprCall(ExprConstr(c), as) =>
        as.zipWithIndex.find { case (a,i) =>
          def fun(n: RenamedNode, subst: Subst) {
            // n = a(map)
            log("-- " + nodeToString(n) + " matches " + a)
            logSubst(this, subst)
            
            addInTrigger(n.node, Construct(c), as.size, i, fun1)
            def fun1(h: Hyperedge) {
              // h.dests(i) = ren comp n = a(ren comp map)
              val ren = h.dests(i).renaming comp n.deref.renaming.inv
              val pairs = for((p,j) <- (h.dests zip as).zipWithIndex; if j != i) yield p
              
              addOutPatternTriggers(pairs, fun2, subst.mapValues(ren comp _))
              def fun2(lst: List[RenamedNode], subst: Subst) {
                // h.source = C(h.dests) = C(as(subst))
                log("-- " + nodeToString(h.source) + " matches " + expr)
                f(h.source, subst)
              }
            }
          }
          addPatternTriggerMB(a, fun)
        }.isDefined
      case _ =>
        false
    }
  }
  
  def addOutPatternTriggers(list: List[(RenamedNode, Expr)], 
        f: (List[RenamedNode], Subst) => Unit, subst: Subst = Map()): Boolean = list match {
    case Nil => 
      f(Nil, subst)
      true
    case (node, expr) :: tail =>
      addOutPatternTrigger(node, expr, { (node, subst) =>
        addOutPatternTriggers(tail, { (lst, subst) => f(node :: lst, subst) }, subst)
      }, subst)
  }
  
  def addOutPatternTrigger(node: RenamedNode, expr: Expr, 
        f: (RenamedNode, Subst) => Unit, subst: Subst = Map()): Boolean = {
    log("-- addOutPatternTrigger " + nodeToString(node) + " =? " + expr)
    expr match {
      case ExprConstr(c) => addOutPatternTrigger(node, ExprCall(expr, Nil), f, subst)
      case ExprFun(name) => 
        addEqPatternTrigger(node, newNode(name, 0), f, subst)
        true
      case ExprCall(ExprFun(name), as) =>
        val fnode = newNode(name, as.size)
        addOutTrigger(node.deref.node, Let(), as.size + 1, fun)
        def fun(h: Hyperedge) {
          addGlueTrigger(h.dests(0).node, fnode.deref.node, fun1)
          def fun1() {
            // h.dests(0) = ren comp fnode
            val ren = h.dests(0).deref.renaming comp fnode.deref.renaming.inv
            // node = name(as(subst)) = ren1 comp h.source
            val ren1 = node.deref.renaming comp h.source.deref.renaming.inv
            val pairs = ren.vector.map(i => ren1 comp h.dests(i + 1)) zip as
            
            addOutPatternTriggers(pairs, fun2, subst)  
            def fun2(lst: List[RenamedNode], subst: Subst) {
              // h.source = name(as(subst))
              log("-- " + nodeToString(h.source) + " matches " + expr)
              logSubst(this, subst)
              f(node.deref, subst)
            }
          }
        }
        
        if(as.forall(_.isInstanceOf[ExprVar]) && as.distinct.size == as.size) {
          addGlueTrigger(node.deref.node, fnode.deref.node, fun3)
          def fun3() {
            // node = ren comp fnode
            val ren = node.deref.renaming comp fnode.deref.renaming.inv
            val pairs = ren.vector.map(variable(_)) zip as
            
            addOutPatternTriggers(pairs, fun4, subst)  
            def fun4(lst: List[RenamedNode], subst: Subst) {
              // node = name(as(subst))
              log("-- " + nodeToString(node) + " matches " + expr)
              logSubst(this, subst)
              f(node.deref, subst)
            }
          }
        }
        
        true
      case ExprCall(ExprConstr(c), as) =>
        addOutTrigger(node.deref.node, Construct(c), as.size, fun)
        def fun(h: Hyperedge) {
          // node = C(as(subst)) = ren comp h.source
          val ren = node.deref.renaming comp h.source.deref.renaming.inv
          val pairs = h.dests.map(ren comp _) zip as
          
          addOutPatternTriggers(pairs, fun1, subst)
          def fun1(lst: List[RenamedNode], subst: Subst) {
            // h.source = name(as(subst))
            log("-- " + nodeToString(h.source) + " matches " + expr)
            logSubst(this, subst)
            f(node.deref, subst)
          }
        }
        true
      case ExprVar(v) =>
        subst.get(v) match {
          case None => f(node, subst + (v -> node))
          case Some(node2) => addEqPatternTrigger(node, node2, f, subst)
        }
        true
      case ExprUnused() =>
        f(node, subst)
        true
      case _ =>
        false
    }
  }
  
  def addEqPatternTrigger(node1: RenamedNode, node2: RenamedNode, 
          f: (RenamedNode, Subst) => Unit, subst: Subst = Map()) {
    log("-- addEqPatternTrigger " + nodeToString(node1) + " =? " + nodeToString(node2))
    addGlueTrigger(node1.deref.node, node2.deref.node, { () =>
      if(node1.deref == node2.deref)
        f(node1.deref, subst)
    })
  }
}
