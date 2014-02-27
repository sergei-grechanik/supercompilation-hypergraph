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
            // n = a(subst)
            log("-- " + nodeToString(n) + " matches " + a)
            logSubst(this, subst)
            
            addInTrigger(n.node, Construct(c), as.size, i, fun1)
            def fun1(h: Hyperedge) {
              // h.dests(i) = ren comp n = a(ren comp subst)
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
      case ExprCaseOf(e0, cases_unsorted) =>
        val cases = cases_unsorted.sortBy(_._1)
        val cases1 = cases.map(x => (x._1, x._2.size)) 
        val es = ((Nil, e0) :: cases.map(x => (x._2, x._3))).zipWithIndex
        es.find { case ((vs,e),i) =>
          def fun(n: RenamedNode, subst: Subst) {
            // n = e(subst)
            log("-- " + nodeToString(n) + " matches " + e)
            logSubst(this, subst)
            
            val vsnodes = for((v,i) <- vs.zipWithIndex; r <- subst.get(v)) yield (r,i)
            addDifferentVarsTrigger(vsnodes.map(_._1), fun1)
            def fun1() {
              // Here we know that subst(vs) are different variables 
              addInTrigger(n.node, CaseOf(cases1), cases1.size + 1, i, fun2)
              def fun2(h: Hyperedge) {
                // h.dests(i) = ren comp n = e(ren comp subst)
                val ren = h.dests(i).renaming comp n.deref.renaming.inv
                
                val varlen = vs.size
                val subst_no_vs = (subst -- vs).mapValues(x => (ren comp x).shiftVars(-varlen))
                // Make sure that subst values don't use bound variables and
                // that vsnodes are variables from 0 to varlen
                if(subst_no_vs.forall(_._2.used.forall(_ >= varlen)) &&
                   vsnodes.forall(p => (ren comp p._1).getVar.get == p._2)) {
                  log("-- " + nodeToString(h.dests(i)) + " matches " + e)
                  log("-- And may be a branch of " + expr)
                  log("-- common subst:")
                  logSubst(this, subst_no_vs)
                  // I was too lazy to write whatever code should go here, 
                  // so I just use an out trigger
                  addOutPatternTrigger(h.source.deref, expr, f, subst_no_vs)
                } else {
                  log("-- " + nodeToString(n) + " matches " + e)
                  log("-- But can't be a branch of " + expr)
                  logSubst(this, subst)
                }
              }
            }
          }
          addPatternTriggerMB(e, fun)
        }.isDefined
      case _ =>
        false
    }
  }
  
  def addDifferentVarsTrigger(list: List[RenamedNode], f: () => Unit): Unit = list match {
    case Nil => f()
    case v::vs=>
      if(!vs.contains(v)) {
        addDifferentVarsTrigger(vs, fun)
        def fun() {
          if(!vs.exists(v ~=~ _))
            addOutTrigger(v.deref.node, Var(), 0, _ => f())
        }
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
    logSubst(this, subst)
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
      case ExprCaseOf(e0, cases_unsorted) =>
        val cases = cases_unsorted.sortBy(_._1)
        val cases1 = cases.map(x => (x._1, x._2.size)) 
        val es = (Nil, e0) :: cases.map(x => (x._2, x._3))
        addOutTrigger(node.deref.node, CaseOf(cases1), cases1.size + 1, fun)
        def fun(h: Hyperedge) {
          // node = case e0(subst) of ... = ren comp h.source
          val ren = node.deref.renaming comp h.source.deref.renaming.inv
          
          val actions =
            for((d, (vs, e)) <-(ren comp h).dests zip es) yield { 
              (next: Subst => Unit, subst: Subst) =>
                val sh = vs.size
                addOutPatternTrigger(d, e, fun1, 
                    subst.mapValues(_.shiftVars(sh)) ++ 
                      vs.zipWithIndex.toMap.mapValues(variable(_)))
                def fun1(r: RenamedNode, newsubst: Subst) {
                  val newsubst_no_vs = newsubst -- vs
                  // Make sure that newsubst values don't use bound variables
                  if(newsubst_no_vs.forall(_._2.used.forall(_ >= sh))) {
                    val ultimate_newsubst = 
                      subst ++ newsubst_no_vs.mapValues(_.shiftVars(-sh))
                    log("-- " + nodeToString(r) + " matches " + e)
                    log("-- which is a branch of " + expr + " with ultimate subst:")
                    logSubst(this, ultimate_newsubst)
                    next(ultimate_newsubst)
                  } else {
                    log("-- " + nodeToString(r) + " matches " + e)
                    log("-- which could be a branch of " + expr)
                    log("-- but it uses bound variables in a wrong way")
                    logSubst(this, newsubst)
                  }
                }
            }
          
          (actions :\ fun2 _)((f,g) => f(g,_))(subst)
          
          def fun2(subst: Subst) {
            log("-- " + nodeToString(node) + " matches " + expr)
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
