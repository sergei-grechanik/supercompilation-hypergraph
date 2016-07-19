package graphsc
package residualization

class SimpleResidualizer(scc: SCC, use_cache: Boolean = true) {
  import CorrectnessChecker._
  
  type S = RSMatrix
  type Hist = List[(Node, S)]
  type HistSet = Set[(Node, S)]
  
  val cache = collection.mutable.Map[Node, 
                 List[(HistSet, Stream[ResidualTree[Node]])]]().withDefaultValue(Nil)
  
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
        val lst = cache(node)
      
        lst.filter(x => use_cache && moreRestrictive(histset, x._1)) match {
          case ress if ress.nonEmpty =>
            ress.find(x => x._2.nonEmpty).map(_._2).getOrElse(Stream.empty)
          case Nil =>
            val res = residualizeUncached(node, hist)
            
            cache(node) = 
              (histset, res) ::
                lst.filterNot(x => moreRestrictive(x._1, histset))
            
            res
        }
    }
  }
    
  def residualizeUncached(node: Node, history: Hist = Nil): Stream[ResidualTree[Node]] = {
    val list_of_streams =
      for(h <- node.outs) yield {
        val smatrices = hyperedgeToMatrix(h)
        val newhisthead = (node, idMatrix(node.deref))
        val dest_residuals =
          for((d, sm) <- h.dests zip smatrices) yield {
            val newhist = (newhisthead :: history).map(x => (x._1, sm * x._2))
            residualize(d.node, newhist)
          }
        sequenceS(dest_residuals).map(l => ResidualTreeNormal(node, h, l))
      }
    return (Stream.empty[ResidualTree[Node]] /: list_of_streams)(_ ++ _)
  }
}

  
