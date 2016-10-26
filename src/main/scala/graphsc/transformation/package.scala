package graphsc

package transformation {

  case class MonoTransformation(run: Hyperedge => Unit)

  object MonoTransformation {
    def fromPartial(fun: PartialFunction[Hyperedge, Unit]): MonoTransformation =
      MonoTransformation(h => fun.lift(h))
  }

  case class BiTransformation(
            run: (Hyperedge, Hyperedge) => Unit, 
            priority_function: (Hyperedge, Hyperedge) => Double,
            prefilter: (Hyperedge, Hyperedge) => Boolean) {

    def modifyPriority(f: (Hyperedge, Hyperedge, Double) => Double): BiTransformation = 
      BiTransformation(run, (h1, h2) => f(h1, h2, priority_function(h1, h2)), prefilter)

    def runMono(h: Hyperedge) {
      // TODO: Should we normalize? or dereference?
      // Note that we used to perform deref here, but not anymore
      // because deref is not enough, we must do either full normalization or nothing at all
      for(d <- h.dests; h1 <- d.deref.node.outs)
        run(h.deref, h1.deref)
      for(h1 <- h.deref.source.node.ins)
        run(h1.deref, h.deref)
    }
  }

  object BiTransformation {
    def fromPartial(fun: PartialFunction[(Hyperedge, Hyperedge), Unit]): BiTransformation = {
      def prefilt(h1o: Hyperedge, h2o: Hyperedge): Boolean = {
        var result: Boolean = false
        for((h1, h2) <- transformablePairs(h1o, h2o))
          result ||= fun.isDefinedAt((h1, h2))
        result
      }

      def run(h1o: Hyperedge, h2o: Hyperedge) {
        val lifted = fun.lift
        var applications: Int = 0
        for((h1, h2) <- transformablePairs(h1o, h2o))
          lifted((h1,h2)) match {
            case Some(_) => applications += 1
            case None =>
          }
        // if(applications == 0)
        //   println("Unapplicable " + prefilt(h1o, h2o))
      }

      BiTransformation(run = run, prefilter = prefilt, priority_function = (_, _) => 0)
    }

    def apply(fun: PartialFunction[(Hyperedge, Hyperedge), Unit]): BiTransformation =
      BiTransformation.fromPartial(fun)
  }

  case class PotentialTransformation(
              h1: Hyperedge, h2: Hyperedge, 
              trans: BiTransformation) {
    def priority: Double = trans.priority_function(h1, h2)
  }
}

package object transformation {
    
  // implicit def partFun2BiTransformation(
  //     fun: PartialFunction[(Hyperedge, Hyperedge), Unit]): BiTransformation =
  //   BiTransformation((h1,h2) => fun((h1,h2)), prefilter = fun.isDefinedAt)

  // implicit def runMono(
  //     fun: PartialFunction[(Hyperedge, Hyperedge), Unit]): Hyperedge => Unit =
  //   partFun2BiTransformation(fun).runMono
    
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
