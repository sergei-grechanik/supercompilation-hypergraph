package graphsc

package transformation {
  trait HProcessor extends Function1[Hyperedge, Unit]
  
  trait BiHProcessor extends Function2[Hyperedge, Hyperedge, Unit] {
    def &(other: BiHProcessor): BiHProcessor = {
      val self = this
      new BiHProcessor {
        override def apply(h1: Hyperedge, h2: Hyperedge) {
          self(h1, h2)
          other(h1, h2)
        }
        
        override def onSuccess(f: () => Unit): BiHProcessor =
          self.onSuccess(f) & other.onSuccess(f)
      }
    }
    
    def cond(c: BiHFilter): BiHProcessor = {
      val self = this
      new BiHProcessor {
        override def apply(h1: Hyperedge, h2: Hyperedge) {
          if(c(h1, h2))
            self(h1, h2)
        }
        
        override def onSuccess(f: () => Unit): BiHProcessor =
          self.onSuccess(f).cond(c)
      }
    }
    
    def onSuccess(f: () => Unit): BiHProcessor
  }
  
  case class BiHProc2HProc(proc: BiHProcessor) extends HProcessor {
    override def apply(h: Hyperedge) {
        // TODO: Should we normalize? or dereference?
        // Note that we used to perform deref here, but not anymore
        // because deref is not enough, we must do either full normalization or nothing at all 
        for(d <- h.dests; h1 <- d.deref.node.outs)
          proc(h.deref, h1.deref)
        for(h1 <- h.deref.source.node.ins)
          proc(h1.deref, h.deref)
    }
  }
  
  case class BFun2BiHProc(funs: List[(Hyperedge, Hyperedge) => Boolean]) 
      extends BiHProcessor {
    override def apply(h1o: Hyperedge, h2o: Hyperedge) {
      for(f <- funs)
        for((h1,h2) <- transformablePairs(h1o.deref, h2o.deref))
          f(h1,h2)
          //funs.foreach(_(h1,h2))
    }
    
    def &(other: BFun2BiHProc): BFun2BiHProc =
      BFun2BiHProc(funs ++ other.funs)
      
    override def onSuccess(f: () => Unit): BFun2BiHProc =
      BFun2BiHProc(funs.map(g => 
        (h1: Hyperedge,h2: Hyperedge) => 
          if(g(h1,h2)) { 
            f(); 
            true 
          } else false))
  }
}

package object transformation {
  type BiHFilter = (Hyperedge, Hyperedge) => Boolean
 
  implicit def biHProc2HProc(p: BiHProcessor): HProcessor =
    BiHProc2HProc(p)
    
  implicit def bFun2BiHProc(fun: (Hyperedge, Hyperedge) => Boolean): BFun2BiHProc =
    BFun2BiHProc(List(fun))
    
  implicit def partFun2BiHProc(fun: PartialFunction[(Hyperedge, Hyperedge), Unit]): BFun2BiHProc =
    BFun2BiHProc(List((h1,h2) => fun.lift((h1,h2)).fold(false)(_ => true)))
    
  def transformablePairs(h1: Hyperedge, h2: Hyperedge): List[(Hyperedge, Hyperedge)] = {
    val Hyperedge(l1, src1, ds1) = h1
    val Hyperedge(l2, src2, ds2) = h2
    // we restore the original used set because it makes it easier to transform
    val node = new Node(src2.used | h2.used)
    node.gluedTo = src2
    val rnode = RenamedNode(src2.renaming.inv, node)
    
    val maxvar = (0 :: h1.dests.map(_.arity)).max + (0 :: h1.shifts).max
    
    // as there may be several occurences of src2 in ds1, we should consider them one by one
    val pairs = 
      for((d,i) <- ds1.zipWithIndex if d.node == src2.node) yield {
        val newds1 = 
          ds1.zipWithIndex.map { 
            case (d,j) => 
              if(i == j) (d.renaming comp rnode).restoreUnused(maxvar)._2
              else d
          }
        (Hyperedge(l1, src1, newds1), Hyperedge(l2, RenamedNode.fromNode(node), ds2))
      }
    
    // We glue it here because otherwise we wouldn't have been able to correctly restore unused
    // variables. We could do without this gluing but some transformations use this dummy node
    // as if it weren't dummy.
    node.gluedTo = src2
    
    pairs
  }
  
  
  type Subst = Map[String, RenamedNode]
  
  def logSubst(g: Hypergraph, subst: Subst) {
    for((s,r) <- subst) g.log("--   " + s + " -> " + g.nodeToString(r))
  }
}
