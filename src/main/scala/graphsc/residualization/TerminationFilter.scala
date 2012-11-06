package graphsc
package residualization

class TerminationFilter[S](graph: Hypergraph)
    (implicit cc: CorrectnessChecker[S]) {
  
  def apply(n: RenamedNode): Option[RenamedNode] =
    go(n.node, Nil).map(n.renaming comp _._1)
  
  private def go(n: Node, hist: List[(Node, S)]): 
      Option[(RenamedNode, List[(Node, RenamedNode)])] = {
    hist.find(_._1 == n) match {
      case Some((_, s)) if cc.safe(s) =>
        val node = graph.newNode(n.used)
        Some((node, List(n -> node)))
      case Some((_, s)) =>
        None
      case None =>
        val newnode = graph.newNode(n.used)
        val maps =
          n.outs.toList.map {
            h =>
              val histnodes = n :: hist.map(_._1)
              val newsafeties = (cc(n.deref) :: hist.map(_._2)).map(cc.through(_, h)).transpose
              val children = 
                h.dests.zip(newsafeties).map { case (d,ss) => go(d.node, histnodes zip ss) }
              if(children.forall(_.isDefined)) {
                val newdests = 
                  children.map(_.get._1).zip(h.dests).map { case (d,old) => old.renaming comp d }
                val maps = children.map(_.get._2)
                graph.add(h.label, h.source.renaming comp newnode.deref, newdests)
                maps
              } else
                Nil
          } flatten
          
        if(newnode.deref.node.outs.nonEmpty) {
          val resmap = 
            (List(n -> newnode.deref) :: maps)
              .flatten.groupBy(_._1).mapValues(l => graph.glue(l.map(_._2)))
          assert(resmap.map(_._1).toSet subsetOf (n :: hist.map(_._1)).toSet)
          Some(resmap(n), resmap.toList.filter(_._1 != n))
        }
        else
          None
    }
  }
}
