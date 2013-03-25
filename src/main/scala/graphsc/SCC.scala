package graphsc

case class SCC(componentOf: Map[Node, Int], components: Map[Int, Set[Node]])

object SCC {
  // Find strongly connected components using Tarjan's algorithm
  implicit def apply(g: Hypergraph): SCC = {
    val compmap = collection.mutable.Map[Node, Int]()
    val components = collection.mutable.Map[Int, Set[Node]]()
    
    val indices = collection.mutable.Map[Node, Int]()
    var index = 0
    val stack = collection.mutable.Stack[Node]()
    
    for(n <- g.allNodes)
      // process all nodes that we didn't reach earlier
      if(!indices.contains(n))
        process(n)
    
    // returns so-called lowlink
    def process(n: Node): Int = {
      // assign a new index to n and push in on the stack
      val myindex = index
      indices(n) = myindex
      index += 1
      stack.push(n)
      
      var lowlink = myindex
      
      for(h <- n.outs; rm <- h.dests; val m = rm.node) {
        indices.get(m) match {
          case None =>
            // We haven't processed m yet, process
            lowlink = lowlink min process(m)
          case Some(i) =>
            // We've already processed m.
            // We have to check if we've assigned an SCC to it.
            if(!compmap.contains(m))
              lowlink = lowlink min indices(m)
        }
      }
      
      if(lowlink == myindex) {
        val compid = components.size
        val comp_stack = collection.mutable.Stack[Node]()
        
        var m: Node = null
        do {
          m = stack.pop()
          comp_stack.push(m)
        } while(m != n)
        
        for(m <- comp_stack)
          compmap += m -> compid
        components += compid -> comp_stack.toSet
      }
      
      lowlink
    }
    
    assert(stack.isEmpty)
    
    SCC(compmap.toMap, components.toMap)
  }
}