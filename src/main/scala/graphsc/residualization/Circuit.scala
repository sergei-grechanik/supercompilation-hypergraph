package graphsc
package residualization

object Circuit {
  type Circuit = List[(Node, Hyperedge, Int)]
  
  def norm(c: Circuit): Circuit = {
    val lst =
      for((i,t) <- c.inits.toList.reverse zip c.tails.toList) yield {
        val nc = t ++ i
        assert(nc.length == c.length)
        (nc, nc.zipWithIndex.map(p => p._1._2.label.hashCode() * p._2).sum)
      }
    lst.minBy(_._2)._1
  }
  
  def backbone(c: Circuit): List[(Label, Int)] =
    c.map(p => (p._2.label, p._3))
  
  // Find elementary circuits using Johnson's algorithm
  def circuits[S](scc: SCC, maxlen: Int = 1000)
                 (implicit cc: CorrectnessChecker[S]): List[Circuit] = {
    val seen = collection.mutable.Set[Node]()
    var result = List[Circuit]()
    var circuits = 0
    var safe_circuits = 0
    
    for((cid, cset) <- scc.components) {
      for(first_node <- cset if !seen(first_node)) {
        val blocked = collection.mutable.Set[Node]()
        val blocks = collection.mutable.Map[Node, Set[Node]]()
        
        def unblock(n: Node) {
          blocked -= n
          blocks.get(n) match {
            case None =>
            case Some(bl) =>
              blocks -= n
              bl.foreach(unblock(_))
          }
        }
        
        def go(s: S, n: Node, hist: List[(Node, Hyperedge, Int)]): Boolean = {
          if(hist.size < maxlen) {
            var good = false
            blocked += n
            for(h <- n.outs; (s1,(d,i)) <- cc.through(s, h) zip h.dests.zipWithIndex) {
              if(d.node == first_node) { 
                circuits += 1
                if(cc.safe(s1)) {
                  safe_circuits += 1
                  result = ((n, h, i) :: hist).reverse :: result
                }
                //print("circuits: " + circuits + "  safe: " + safe_circuits + "\r")
                good = true
              }
              else if(cset(d.node) && !seen(d.node) && !blocked(d.node)) 
                if(go(s1, d.node, (n, h, i) :: hist))
                  good = true
            }
            if(good) unblock(n)
            else 
              for(h <- n.outs; d <- h.dests; if cset(d.node) && !seen(d.node))
                blocks += (d.node -> (blocks.getOrElse(d.node, Set()) + n))
            good
          } else true
        }
        
        go(cc(first_node.deref), first_node, Nil)
        seen += first_node
      }
    }
    
    result
  }
}