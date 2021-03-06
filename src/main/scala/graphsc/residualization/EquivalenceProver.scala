package graphsc
package residualization

class EquivalenceProver[S, L](scc: SCC, likenesscalc: LikenessCalculator, 
                              use_cache:Boolean = true) {
  import CorrectnessChecker._
  import likenesscalc._
  
  type S = RSMatrix
  type Hist = List[((Node, S), (Node, S))]
  type HistSet = Set[((Node, S), (Node, S))]
  
  val checkedIndependently = collection.mutable.Set[(Node, Node)]()
  
  val cache = collection.mutable.Map[(Node, Node), 
                 List[((Renaming, HistSet), Option[EqProofTree])]]().withDefaultValue(Nil)
  
  var stats = collection.mutable.Map[(Node, Node), Int]()
  
  def filterUnreachable(n: Node, ns: Set[Node]): Set[Node] = {
    val visited = collection.mutable.Set[Node]()
    val res = collection.mutable.Set[Node]()
    
    def go(n: Node) {
      if(!visited(n)) {
        visited += n
        if(ns(n)) {
          res += n
        } else {
          for(h <- n.outs; m <- h.dests)
            go(m.node)
        }
      }
    }
    
    go(n)
    res.toSet
  }
  
  // Advanced history filtering: doesn't seem to be very useful
  def filterHistory(l: Node, r: Node, h: Hist): Hist = {
    val lreach = filterUnreachable(l, h.map(_._1._1).toSet)
    val rreach = filterUnreachable(r, h.map(_._2._1).toSet)
    h.filter(x => lreach(x._1._1) && rreach(x._2._1))
  }
  
  def moreRestrictive(more: (Renaming, HistSet), less: (Renaming, HistSet)): Boolean = {
    less._1.toMap.toSet.subsetOf(more._1.toMap.toSet) &&
    less._2.subsetOf(more._2)
  }
  
  def prove(
      l1: Node, 
      r1: Node, 
      ren1: Renaming = Renaming(), 
      hist2: Hist = Nil): Option[EqProofTree] = {
    
    val hist1 = 
      if(scc != null) {
        val lcompid = scc.componentOf(l1)
        val rcompid = scc.componentOf(r1) 
        hist2.takeWhile(p => 
          scc.componentOf(p._1._1) == lcompid && scc.componentOf(p._2._1) == rcompid) 
      } else {
        hist2
      }
    
    val lind = hist1.indexWhere(_._1._1 == l1)
    val rind = hist1.indexWhere(_._2._1 == r1)

    val renid = (ren1 | Renaming(r1.used))
    
    if(l1.deref ~=~ (ren1 comp r1))
      Some(EqProofTree(ren1, (l1,r1)))
    else if(l1 == r1 && renid.nonEmpty)
      renid.map(EqProofTree(_, (l1,r1)))
    else if(lind != -1 && rind != -1) {
      hist1.find(p => p._1._1 == l1 && p._2._1 == r1) match {
        case Some(((_, lsafety), (_, rsafety))) if lsafety.locallySafe && rsafety.locallySafe =>
          // I thought we should add some variables to ren (if they guarantee correctness)
          // But now I think they will be there if they guarantee correctness.
          Some(EqProofTree(ren1, (l1,r1)))
        case _ => None
      }
    } else if(lind != -1 || rind != -1) {
      // Well, we trade precision for efficiency here
      None
    //} else if(l1.used.size != r1.used.size) {
    //  None
    } else {
      val swap = l1.hashCode > r1.hashCode
      
      // Advanced history filtering: doesn't seem to be very useful
      //val hist0 = filterHistory(l1, r1, hist1)
      
      val args@(l, r, ren, hist) =
        if(!swap) (l1, r1, ren1, hist1)
        else {
          val (lh,rh) = hist1.unzip
          (r1, l1, ren1.inv, rh zip lh)
        }
      
      val histset = hist.toSet
      
      // Trying to prove l = r if we haven't tried yet doesn't seem to be useful
//      if(!checkedIndependently.contains((l,r))) {
//        checkedIndependently.add((l,r))
//        prove(l, r)
//      }
      
      val lst = cache((l,r))
      
      (lst.filter(x => use_cache &&
          moreRestrictive((ren, histset), x._1) && 
          x._2.forall(t => (t.renaming | ren).isDefined)) match {
        case ress if ress.nonEmpty =>
          ress.find(x => x._2.nonEmpty).map(_._2).getOrElse(None)
        case Nil =>
          val res = proveUncached(l, r, ren, hist)
          
          if(res == None && likeness(l.deref,r.deref) != None) {
            if(stats.contains((l,r)))
              stats((l,r)) += 1
            else
              stats += (l,r) -> 1
          }
          
          cache((l,r)) = 
            ((ren,histset), res) ::
              lst.filterNot(x => moreRestrictive(x._1, (ren,histset)) &&
                                  (x._2, res).zipped.forall((t1,t2) => 
                                    t1.renaming.toMap.toSet.subsetOf(t2.renaming.toMap.toSet)))
          
          res
      }).map(t => if(swap) t.swap else t)
    }
  }
    
  def proveUncached(
      l: Node, 
      r: Node, 
      ren: Renaming = Renaming(), 
      hist: Hist = Nil): Option[EqProofTree] = {
    //println("EQ " + this.hashCode() + " " + l.uniqueName + " " + r.uniqueName + " " + ren)
    val louts = l.outs.groupBy(_.label)
    val routs = r.outs.groupBy(_.label)
    
    val pairs = 
      for((llab,lset) <- louts.iterator; (rlab,rset) <- routs.iterator; if llab == rlab; 
          lh <- lset; rh1 <- rset; 
          if lh.dests.size == rh1.dests.size;
          rh <- varySecond(lh, rh1)) yield
        likenessH(lh, rh, ren).map{ case (like, rn) => (like, lh, rh, rn) }
    
    val sorted_pairs = 
      pairs.collect{ case Some(p) => p }.toList.distinct.sortBy(-_._1)
    
    var result: Option[Renaming] = None
    var hypers: (Hyperedge, Hyperedge) = null
    var subtrees: List[EqProofTree] = null
      
    // for each pair of hyperedges while there is no result
    for((like, lh, rh, ren1) <- sorted_pairs; if result == None) {
      subtrees = Nil
      
      val lh_matrices = hyperedgeToMatrix(lh)
      val rh_matrices = hyperedgeToMatrix(rh)
      
      val newhisthead = ((l, idMatrix(l.deref)), (r, idMatrix(r.deref)))
      val newss1 =
        (newhisthead :: hist).map(p => lh_matrices.map(_ * p._1._2)).transpose
      val newss2 = 
        (newhisthead :: hist).map(p => rh_matrices.map(_ * p._2._2)).transpose
        
      var curren: Option[Renaming] = 
        Some(lh.source.renaming comp ren1 comp rh.source.renaming.inv)
      
      val lhs = lh.dests zip newss1 zip lh.shifts
      val rhs = rh.dests zip newss2
      
      for((((ld, news1), sh), (rd, news2)) <- lhs zip rhs if curren != None) {
        val newhist =
          ((newhisthead :: hist) zip (news1 zip news2)).map {
            case (((n1,_), (n2,_)), (s1, s2)) => 
              ((n1, s1), (n2, s2)) 
          }
        
        if(sh != -1) {
          // it is a normal or shifted child
          val shiftedren = curren.get.shift(sh) | Renaming(0 until sh toSet)
          
          val subtree =
            shiftedren.flatMap(shren =>
              prove(ld.node, rd.node, 
                    ld.renaming.inv comp shren comp rd.renaming, 
                    newhist))
          
          subtree.foreach { t => subtrees = t :: subtrees }     
                  
          curren = curren |
            subtree.map(t => (ld.renaming comp t.renaming comp rd.renaming.inv).unshift(sh))
        }
        else {
          // It's let's head
          assert(lh.label == Let())
          
          // We cannot judge what variables are used if we've got a renaming from
          // the equivalence prover. Just FYI.
          prove(ld.node, rd.node, ld.renaming.inv comp rd.renaming, newhist) match {
            case None => curren = None
            case Some(t) => subtrees = t :: subtrees 
          }
        }
      }
      
      result = curren.map(lh.source.renaming.inv comp _ comp rh.source.renaming)
      hypers = (lh, rh)
    }
      
    result.map(resren => 
      EqProofTree(resren, (l,r), Some((hypers._1, hypers._2, subtrees.reverse))))
  }
  
  // Since two let-expressions can match with some non-id rearrangement,
  // we should check them all.
  // This function builds a list of all viable rearranged versions of rh.
  def varySecond(lh: Hyperedge, rh: Hyperedge): List[Hyperedge] = {
    assert(lh.label == rh.label && lh.dests.size == rh.dests.size)
    if(lh.label != Let())
      List(rh)
    else {
      likeness(lh.dests(0), rh.dests(0)) match {
        case None => List()
        case Some((_, headren)) =>
          // this function creates a list of rearranged dest lists together with
          // corresponding rearranging renamings
          def nextDests(tail: List[(RenamedNode, Int)], rest: List[(RenamedNode, Int)]): 
            List[(List[RenamedNode], Renaming)] = tail match {
            case Nil => List((Nil, Renaming()))
            case (ld, i) :: tail1 =>
              // we try to pair the head of the rest part of the left hand side dests
              // with each of the node from the rest part of the right hand side dests
              (for((rd, j) <- rest 
                  if (headren(j) == -1 || headren(j) == i) &&
                     likeness(ld, rd) != None
                ) yield {
                // we've chosen the next node from the rhs dests,
                // do a recursive call and combine the renamings
                val nexts = nextDests(tail1, rest.filterNot(_._2 == j))
                nexts.map { case (ds, ren) => (rd :: ds, (ren | Renaming(j -> i)).get) }
              }).flatten
          }
          
          nextDests(lh.dests.tail.zipWithIndex, rh.dests.tail.zipWithIndex).map {
            case (ds, ren) =>
              Hyperedge(rh.label, rh.source, (ren comp rh.dests(0)) :: ds)
          }
      }
    }
  }
}

case class EqProofTree(
    renaming: Renaming, 
    nodes: (Node, Node), 
    out: Option[(Hyperedge, Hyperedge, List[EqProofTree])] = None) {
  
  def swap: EqProofTree =
    EqProofTree(renaming.inv, (nodes._2, nodes._1), 
        out.map { case (h1, h2, lst) => (h2, h1, lst.map(_.swap)) })
  
  // Why can we glue child nodes too, not only roots?
  // Because they are defined in terms of themselves and the roots.
  def performGluing(g: Hypergraph) {
    // The first thing to do is to fix renamings
    propagateRenamings.performGluing1(g)
  }
  
  private def performGluing1(g: Hypergraph) {
    for((_,_,l) <- out; t <- l)
      t.performGluing(g)
    g.add(Id(), renaming comp nodes._2, List(nodes._1.deref))
  }
  
  // Fix renamings. When we prove equivalence we cannot infer correct renamings,
  // so we should essentially perform a data-flow analysis. 
  def propagateRenamings: EqProofTree = {
    var prev = this
    var cur = propagateRenamings1()
    while(cur != prev) {
      prev = cur
      cur = cur.propagateRenamings1()
    }
    cur
  }
  
  // One iteration of propagating renamings up
  private def propagateRenamings1(hist: List[EqProofTree] = Nil): EqProofTree = 
    out match {
      case None =>
        hist.find(t => t.nodes == nodes) match {
          case None =>
            // this node is refl in agda terms, we don't have to modify it since
            // equivalence prover use the set of used variables without questioning it
            this
          case Some(t) =>
            // it is a folding node, we should combine both renamings
            EqProofTree((t.renaming | renaming).get, nodes, None)
        }
      case Some((h1, h2, ds)) =>
        // recursively perform renaming propagation below
        val newds = ds.map(_.propagateRenamings1(this :: hist))
        // now lift through hyperedges and combine all the renamings
        var ren = Renaming()
        for((((d1, d2), s), t) <- h1.dests zip h2.dests zip h1.shifts zip newds if s != -1) {
          // if the renamings are uncombinable then something has gone wrong
          ren = (ren | (d1.renaming comp t.renaming comp d2.renaming.inv).unshift(s)).get
        }
        EqProofTree(
            h1.source.renaming.inv comp ren comp h2.source.renaming, 
            nodes, Some((h1, h2, newds)))
    }
  
  def leftTree: ResidualTree =
    if(out == None && nodes._1 == nodes._2) ResidualTreeStop(nodes._1)
    else if(out == None) ResidualTreeFold(nodes._1)
    else ResidualTreeNormal(nodes._1, out.get._1, out.get._3.map(_.leftTree))
  def rightTree: ResidualTree =
    if(out == None && nodes._1 == nodes._2) ResidualTreeStop(nodes._2)
    else if(out == None) ResidualTreeFold(nodes._2)
    else ResidualTreeNormal(nodes._2, out.get._2, out.get._3.map(_.rightTree))
  
  // Check overall correctness. Isolated folding correctness, which is checked
  // during the graph traversal, usually is not enough.
  def checkCorrectness: Boolean =
    leftTree.checkCorrectness && rightTree.checkCorrectness
  
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
    g.log("-- " + g.nodeToString(nodes._1.deref) + " = " + 
            g.nodeToString(renaming comp nodes._2.deref))
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

sealed trait ResidualTree {
  val node: Node
  
  def checkCorrectness: Boolean = {
    var curind = 0
    val graph = collection.mutable.Set[(Int, Int, RSMatrix)]()
    
    def mkGraph(r: ResidualTree, hist: List[(Node, Int)]): Option[Int] = r match {
      case ResidualTreeFold(n) => hist.find(_._1 == n).map(_._2)
      case ResidualTreeStop(_) => None
      case ResidualTreeNormal(n, h, chld) =>
        var recursive = false
        val myind = curind
        curind += 1
        val mats = CorrectnessChecker.hyperedgeToMatrix(h)
        for((m, Some(j)) <- mats zip chld.map(mkGraph(_, (n, myind) :: hist))) {
          recursive = true
          graph += ((myind, j, m))
        }
        
        if(recursive) Some(myind) else None
    }
    
    mkGraph(this, Nil)
    
//    for((i,j,m) <- graph) {
//      println(i + " -> " + j)
//      for(row <- m.mat)
//        println(row.mkString(" "))
//    }
//    println("")
    
    CorrectnessChecker.checkCallGraph(graph.toSet)
  }
}

case class ResidualTreeNormal(node: Node, hyperedge: Hyperedge, children: List[ResidualTree]) 
  extends ResidualTree
case class ResidualTreeFold(node: Node) extends ResidualTree
case class ResidualTreeStop(node: Node) extends ResidualTree
  
