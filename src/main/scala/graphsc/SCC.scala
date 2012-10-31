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
      if(!indices.contains(n))
        process(n)
    
    // returns so-called lowlink
    def process(n: Node): Int = {
      val myindex = index
      indices(n) = myindex
      index += 1
      stack.push(n)
      
      var lowlink = index
      
      for(h <- n.outs; rm <- h.dests; val m = rm.node) {
        indices.get(m) match {
          case None =>
            lowlink = lowlink min process(m)
          case Some(i) =>
            if(stack.contains(m))
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
    
    SCC(compmap.toMap, components.toMap)
  }
}