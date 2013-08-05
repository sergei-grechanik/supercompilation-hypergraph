package graphsc
package transformation

class Generalizer(scc: SCC = null) {
  type Hist = List[(Node, Node)]
  
  val cache = collection.mutable.Map[(Node, Node, Renaming, Hist), List[Generalization]]()
  
  /*
  def generalize(
      l1: Node, 
      r1: Node, 
      ren1: Renaming = Renaming(), 
      hist2: Hist = Nil): List[Generalization] = {
    
    val hist1 = 
      if(scc != null) {
        val lcompid = scc.componentOf(l1)
        val rcompid = scc.componentOf(r1) 
        hist2.takeWhile(p => 
          scc.componentOf(p._1) == lcompid && scc.componentOf(p._2) == rcompid) 
      } else {
        hist2
      }
    
    val swap = l1.hashCode > r1.hashCode
    
    val args@(l, r, ren, hist) =
      if(!swap) (l1, r1, ren1, hist1)
      else {
        val (lh,rh) = hist1.unzip
        (r1, l1, ren1.inv, rh zip lh)
      }
    
    cache.getOrElseUpdate(args, {
      val res = generalizeUncached(l, r, ren, hist)
      res
    }).map(t => if(swap) t.swap else t)
  }*/
    
  def generalize(
      l: RenamedNode, 
      r: RenamedNode, 
      ren: Renaming = Renaming(),
      lren1: Renaming = null,
      rren1: Renaming = null,
      hist: Hist = Nil): List[Generalization] =
        generalizeNodes(l.node, r.node, l.renaming.inv comp ren comp r.renaming, 
            lren1 comp l.renaming, rren1 comp r.renaming, hist)
  
  def generalizeNodes(
      l: Node, 
      r: Node, 
      ren: Renaming = Renaming(),
      lren1: Renaming = null,
      rren1: Renaming = null,
      hist: Hist = Nil): List[Generalization] = {
    val lren = if(lren1 == null) Renaming(l.used) else lren1
    val rren = if(rren1 == null) Renaming(r.used) else rren1
    
    val lind = hist.indexWhere(_._1 == l)
    val rind = hist.indexWhere(_._2 == r)
    
    val renid = (ren | Renaming(r.used))
    
    val bycoupl =
      if(l.deref ~=~ (ren comp r))
        List(Generalization(ren, (l,r), (lren, rren)))
      else if(l == r && renid.nonEmpty)
        renid.map(Generalization(_, (l,r), (lren, rren))).toList
      else if(lind != -1 || rind != -1)
        Nil
      else {
        val louts = l.outs.groupBy(_.label)
        val routs = r.outs.groupBy(_.label)
        
        // pairs of hyperedges
        val pairs = 
          for((llab,lset) <- louts.iterator; (rlab,rset) <- routs.iterator; if llab == rlab; 
              lh <- lset; rh <- rset; if lh.dests.size == rh.dests.size) yield (lh, rh)
          
        // TODO: shuffle lets
        (for((lh, rh) <- pairs) yield {
          val curren = lh.source.renaming comp ren comp rh.source.renaming.inv
          
          sequence(for(((ld, sh), rd) <- lh.dests zip lh.shifts zip rh.dests) yield {
            val newhist = (l, r) :: hist
            
            if(sh != -1) {
              // it is a normal or shifted child
              val shiftedren = curren.shift(sh)
              
              val shiftedlren = Renaming((0 until sh).toList ++ lren.vector)
              val shiftedrren = Renaming((0 until sh).toList ++ rren.vector)
              
              generalize(ld, rd, shiftedren, shiftedlren, shiftedrren, newhist)
            }
            else {
              // It's let's head
              assert(lh.label == Let())
              
              val newlren = Renaming(lh.dests.tail.map(_.getVar.getOrElse(-1)))
              val newrren = Renaming(rh.dests.tail.map(_.getVar.getOrElse(-1)))
              
              generalize(ld, rd, Renaming(rd.used), newlren, newrren, newhist)
            }
          }).flatMap { lst =>
            val rens =
              for((rn,sh) <- lst.map(_.renaming) zip lh.shifts)
                yield if(sh == -1) Renaming() else rn.unshift(sh)
            val ren = 
              (Some(Renaming()).asInstanceOf[Option[Renaming]] /: rens)((a,b) => a.flatMap(_ | b))
            ren.toList.map(rn =>
              Generalization(
                lh.source.renaming.inv comp rn comp rh.source.renaming,
                (l, r), (lren, rren), Some((lh, rh, lst))))
          }
        }).toList.flatten
      }
    
    // If there are no equal hyperedges or l and r aren't equal,
    // we should factor l and r out if possible.
    if(bycoupl.isEmpty && 
       l.used.subsetOf(lren.domain) &&
       r.used.subsetOf(rren.domain) &&
       lren.vector.forall(i => i != -1 || !l.used(i)) &&
       rren.vector.forall(i => i != -1 || !r.used(i)) &&
       // TODO: This condition should be removed or relaxed
       !hist.contains((l, r))) {
      List(Generalization(ren, (l, r), (lren, rren)))
    }
    else
      bycoupl
  }
}


case class Generalization(
    renaming: Renaming, 
    nodes: (Node, Node),
    rens: (Renaming, Renaming),
    out: Option[(Hyperedge, Hyperedge, List[Generalization])] = None) {
  
  lazy val depth: Int = out match {
    case None => 1
    case Some((_,_,l)) => 1 + (0 :: l.map(_.depth)).max
  }
  
  def swap: Generalization =
    Generalization(renaming.inv, (nodes._2, nodes._1), (rens._2, rens._1), 
        out.map { case (h1, h2, lst) => (h2, h1, lst.map(_.swap)) })
  
  def performGeneralization(graph: Hypergraph): (Hyperedge, Hyperedge) = {
    val (n, map1) = performGeneralizationImpl(graph)
    val map = map1.mapValues { case (a,b) => (glueVariables(a, graph), glueVariables(b, graph))}
    val ar = (n.arity :: map.keys.toList).max
    val h1 =
      graph.addH(Let(), 
          n :: (0 to ar).toList.map(i => map.get(i).fold(graph.variable(i))(_._1)))
    val rinv = renaming.inv
    val h2 =
      graph.addH(Let(), 
          n :: (0 to ar).toList.map(i => map.get(i).fold(graph.variable(rinv(i)))(_._2)))
    (h1._2, h2._2)
  }
  
  def glueVariables(n: RenamedNode, graph: Hypergraph): RenamedNode = {
    if(n.isInvertible)
      n
    else
      graph.add(Let(), 
          n.node.deref :: (0 to n.node.arity).toList.map(i => graph.variable(n.renaming(i))))
  }
        
  def performGeneralizationImpl(graph: Hypergraph): 
      (RenamedNode, Map[Int, (RenamedNode, RenamedNode)]) = out match {
    case Some((h1,h2,gs)) =>
      val ds = gs.map(_.performGeneralizationImpl(graph))
      val maxvar = (0 :: ds.map(_._1.arity)).max + 1
      var pairs = ds.flatMap(_._2.values).distinct
      
      val unused = graph.unused
      
      val unused_i =
        pairs.indexWhere(p => p._1 ~~ unused && p._2 ~~ unused) match {
          case -1 => 
            pairs = (unused, unused) :: pairs
            0
          case j => j
        } 
      
      val newdests =
        for(((old,sh),(d,map)) <- h1.dests zip h1.shifts zip ds) yield {
          val newren =
            Renaming((0 until d.arity).toList.map { i =>
              map.get(i) match {
                case Some(p) => maxvar + (sh max 0) + pairs.indexOf(p)
                case None => old.renaming(i)
              }
            })
          newren comp d
        }
      
      val newnode =
        if(h1.label == Let()) {
          val vars = 
            ((newdests.length - 1) until (maxvar + pairs.length)).toList.map(graph.variable(_))
          graph.add(h1.label, newdests ++ vars)
        }
        else {
          graph.add(h1.label, newdests)
        }
      
      val h1_srcren_inv = h1.source.renaming.inv
      val srcren_maxvar = (0 :: h1_srcren_inv.vector).max + 1 
      val newren = 
        Renaming((0 until ((maxvar + pairs.length) max newnode.arity)).toList.map { i =>
          if(i < maxvar) h1_srcren_inv(i) match {
            case -1 =>
              if(!newnode.used(i)) -1
              else unused_i + srcren_maxvar
            case j => j
          }
          else i - maxvar + srcren_maxvar
        })
      val newmap = pairs.zipWithIndex.map{ case (p,i) => (i + srcren_maxvar, p) }.toMap
      
      assert((newren comp newnode).isInvertible)
      (newren comp newnode, newmap)
    case None if nodes._1 == nodes._2 => (nodes._1.deref, Map())
    case None =>
      (graph.variable(0), Map(0 -> (rens._1 comp nodes._1, rens._2 comp nodes._2)))
  }
        
  def toDot: String = {
    val label =
      nodes._1 + "\\l" + renaming + "\\l" + nodes._2 + "\\l\\l" +
      nodes._1.prettyDebug.replace("\n", "\\l") + "\\l\\l" +
      nodes._2.prettyDebug.replace("\n", "\\l") + "\\l"
    "\"" + nodes.toString + "\"" + "[label=\"" + label + "\",shape=box];\n" +
    (out match {
      case None => ""
      case Some((h1, h2, subs)) =>
        "\"" + (h1,h2).toString + "\"" + "[label=\"{" + h1.label + "|{" + 
          subs.indices.map("<" + _ + ">").mkString("|") + "}}\",shape=record];" +
        subs.zipWithIndex.map{ case (t,i) =>
            "\"" + (h1,h2).toString + "\":" + i + " -> " + "\"" + t.nodes.toString() + "\";\n"
          }.mkString("\n") +
        "\"" + nodes.toString + "\"" + " -> " + "\"" + (h1,h2).toString + "\"" + ";\n" +
        subs.map(_.toDot).mkString("\n")
    })
  }
  
  def toLog(g: Hypergraph) {
    g.log("-- " + g.nodeToString(nodes._1.deref) + " <> " + 
            g.nodeToString(nodes._2.deref))
    g.log("-- " + renaming)
    g.log("-- " + rens._1 + " <> " + rens._2)
    out match {
      case None =>
      case Some((h1, h2, lst)) =>
        g.logShift()
        g.log("-- " + g.hyperedgeToString(h1))
        g.log("-- " + g.hyperedgeToString(h2))
        g.logShift()
        lst.foreach(_.toLog(g))
        g.logUnshift()
        g.logUnshift()
    }
  }
    
}
