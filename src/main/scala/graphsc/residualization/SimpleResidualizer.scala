package graphsc
package residualization

class SimpleResidualizer(scc: SCC, use_cache: Boolean = true) {
  import CorrectnessChecker._
  
  type S = RSMatrix
  type Hist = List[(Node, S)]
  type HistSet = Set[(Node, S)]
  
  val cache = collection.mutable.Map[(Node, HistSet), Stream[ResidualTree[Node]]]()
  
  def moreRestrictive(more: HistSet, less: HistSet): Boolean =
    less.subsetOf(more)
  
  def residualize(node: Node, history: Hist = Nil): Stream[ResidualTree[Node]] = {
    
    val hist = 
      if(scc != null) {
        val compid = scc.componentOf(node)
        history.takeWhile(p => scc.componentOf(p._1) == compid) 
      } else {
        history
      }
    
    hist.find(p => p._1 == node) match {
      case Some((_, safety)) if safety.locallySafe =>
        Stream(ResidualTreeFold(node))
      case Some(_) => Stream.empty
      case None =>
        val histset = hist.toSet
        cache.get((node, histset)) match {
          case Some(s) => s
          case None =>
            val res = residualizeUncached(node, hist)
            cache += ((node, histset) -> res)
            res
        }
    }
  }
    
  def residualizeUncached(node: Node, history: Hist = Nil): Stream[ResidualTree[Node]] = {
    val outs = 
      node.outs.toList.sortBy(h => h.label match {
        case _ if isDefining(h) => 0
        case _ => 1 + h.dests.length
      })

    val stream_of_streams =
      for(h <- outs.toStream) yield {
        val smatrices = hyperedgeToMatrix(h)
        val newhisthead = (node, idMatrix(node.deref))
        val dest_residuals =
          for((d, sm) <- h.dests zip smatrices) yield {
            val newhist = (newhisthead :: history).map(x => (x._1, sm * x._2))
            residualize(d.node, newhist)
          }
        sequenceS(dest_residuals).map(l => ResidualTreeNormal(node, h, l))
      }
    return stream_of_streams.flatten
  }
}

  
