package graphsc
package transformation

class Supercompilation(graph: Transformations, protect: Set[Node], maxdepth: Int = 10) {
  def supercompile(n: Node) {
    val visited = collection.mutable.Set[Node]()
    
    def go(n: Node, hist: List[Node]) {
      if(!visited.exists(_ ~~ n)) {
        visited += n
        val steps = makeSteps(n, hist, visited.toSet).map(graph.normalize(_)).distinct
        //System.err.println("hyperedges after steps: " + graph.allHyperedges.size)
        graph.log("-- Supercompilation steps: " + steps.length)
        graph.log("-- from node: " + graph.nodeToString(n.deref))
        graph.log("")
        for(h <- steps; d <- h.dests)
          goReduceCalls(d.node, 0)
        for(h <- steps; d <- h.dests) {
          graph.log("-- from node: " + graph.nodeToString(n.deref) + " = " + n.prettyDebug)
          graph.log("-- Supercompilation hyper: " + graph.hyperedgeToString(h))
          graph.log("-- Supercompilation dest: " + graph.nodeToString(d) + "\n")
          go(d.node, n :: hist)
        }
      }
    }

    def goReduceCalls(n: Node, depth: Int) {
      if(depth <= maxdepth && !protect.exists(_ ~~ n)) {
        var reduced = false
        for(h1@Hyperedge(Let(), _, f :: _) <- n.deref.node.outs;
            if protect.exists(_ ~~ f.deref.node);
            d <- h1.dests) {
          reduced = true
        }
        for(h1@Hyperedge(l, _, f :: _) <- n.deref.node.outs;
            if l != Let(); 
            d <- h1.dests) {
          reduced = true
        }
        if(!reduced)
          for(h1@Hyperedge(Let(), _, d :: _) <- n.deref.node.outs; h2 <- d.node.outs) {
            //System.err.println("hyperedges: " + graph.allHyperedges.size)
            graph.letIfNotProtected(protect).run(h1, h2)
          }
      }
    }
    
    go(n, Nil)
  }
  
  def makeSteps(n: Node, hist: List[Node], visited: Set[Node]): List[Hyperedge] = {
    if(!whistle(n, hist, visited))
      drivingSteps(n)
    else 
      Nil
  }
  
  def whistle(n: Node, hist: List[Node], visited: Set[Node]): Boolean =
    hist.size > maxdepth
  
  def drivingSteps(n: Node): List[Hyperedge] =
    graph.drive(n).toList
    
  def generalizationSteps(n: Node, hist: List[Node]): List[Hyperedge] = {
    val generalizer = new Generalizer(graph)
    val gens = hist.map(m => generalizer.generalizeNodes(n.deref.node, m.deref.node)).flatten
    (for(g <- gens.sortBy(-_.depth).filter(_.depth > 1).take(1)) yield {
      val p = g.performGeneralization(graph)
      List(p._1, p._2) 
    }).flatten
  }
}
