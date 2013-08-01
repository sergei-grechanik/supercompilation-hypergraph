package graphsc
package transformation

class Supercompilation(graph: Transformations, maxdepth: Int = 10) {
  def supercompile(n: Node) {
    val visited = collection.mutable.Set[Node]()
    
    def go(n: Node, hist: List[Node]) {
      if(!visited.exists(_ ~~ n)) {
        visited += n
        val steps = makeSteps(n, hist, visited.toSet).map(graph.normalize(_)).distinct
        graph.log("-- Supercompilation steps: " + steps.length)
        graph.log("-- from node: " + graph.nodeToString(n.deref))
        graph.log("")
        for(h <- steps; d <- h.dests) {
          graph.log("-- from node: " + graph.nodeToString(n.deref) + " = " + n.prettyDebug)
          graph.log("-- Supercompilation hyper: " + graph.hyperedgeToString(h))
          graph.log("-- Supercompilation dest: " + graph.nodeToString(d) + "\n")
          go(d.node, n :: hist)
        }
      } 
    }
    
    go(n, Nil)
  }
  
  def makeSteps(n: Node, hist: List[Node], visited: Set[Node]): List[Hyperedge] = {
    val gsteps = generalizationSteps(n, hist)
    if(gsteps.nonEmpty)
      gsteps
    else
      drivingSteps(n)
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
