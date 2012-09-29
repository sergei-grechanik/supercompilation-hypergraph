package graphsc
import scala.util.Random

trait Hypergraph {
  // h should connect nodes known to the hypergraph
  // h.source may be a free node, in this case the function should add or find an appropriate node 
  // This function may perform transformations
  def addHyperedge(h: Hyperedge): RenamedNode
  
  // add a hyperedge, shortcut
  def add(l: Label, src: RenamedNode, ds: List[RenamedNode]): RenamedNode =
    addHyperedge(Hyperedge(l, src, ds))
  
  // Create a node with a hyperedge (or return an existing one)
  def add(l: Label, ds: List[RenamedNode]): RenamedNode =
    addHyperedge(Hyperedge(l, null, ds).freeSource)
  
  // Create a node without any connections
  def newNode(used: Set[Int]): RenamedNode
  
  // Nodes shouldn't be glued manually, they should be marked equal with 
  // an Id() hyperedge. Then the hypergraph should glue these nodes automatically.
  // def glueNodes(l: Node, r: Node): Node
  
  def onUsedReduced(n: Node) {}
  def onNewHyperedge(h: Hyperedge) {}
  def beforeGlue(l: RenamedNode, r: Node) {}
  def afterGlue(l: Node) {}
  
  def glue(l: List[RenamedNode]): RenamedNode = l match {
    case List(r) => r
    case n1 :: n2 :: t => glue(add(Id(), n1, List(n2)) :: t) 
    case Nil => 
      throw new RuntimeException("List of nodes to glue must be non-empty")
  }
  
  // deprecated
  def allNodes: Set[Node] =
    null
  
  // RenamedNode representing ith variable
  def variable(i: Int): RenamedNode = {
    val res = Renaming(0 -> i) comp add(Var(), Nil)
    assert(res.used.size == 1)
    res
  }
    
  // Hyperedge normalizer
  def normalize(h_underef: Hyperedge): Hyperedge = {
    val h = h_underef.deref
    h.label match {
      case Let() => 
        val newhead = h.dests(0).plain
        val newtail = 
          (0 until newhead.arity toList).map { i =>
            h.dests(0).renaming(i) match {
              case -1 => add(Error(), Nil)
              case j => h.dests.tail(j)
            }
          }
        
        if(newtail.isEmpty)
          Hyperedge(Id(), h.source, List(newhead))
        else
          Hyperedge(Let(), h.source, newhead :: newtail)
      case _ => h
    }
  }
  
  def canonize(ns: List[RenamedNode], shifts: List[Int]): (Renaming, List[RenamedNode]) = {
    require(ns.size == shifts.size)
    val varlist = (ns zip shifts).map {
      case (n,s) => n.renaming.vector.map(_ - s).filter(_ >= 0) }.flatten.distinct
    val map = varlist.zipWithIndex.toMap
    
    def varmap(i: Int): Int = 
      if(i < 0) i else map(i)
    
    (Renaming(varlist), (ns zip shifts).map { 
        case (RenamedNode(r,n),s) =>
          RenamedNode(Renaming(r.vector.map(i => varmap(i - s) + s)), n)
      })
  }
  
  def canonize(h: Hyperedge): (Renaming, Hyperedge) = h.label match {
    case CaseOf(cases) =>
      val (r, newds) = canonize(h.dests, 0 :: cases.map(_._2))
      (r, Hyperedge(CaseOf(cases), r.inv comp h.source, newds))
    case Let() =>
      val (r, newtail) = canonize(h.dests.tail, h.dests.tail.map(_ => 0))
      (r, Hyperedge(Let(), r.inv comp h.source, h.dests(0) :: newtail))
    case Var() =>
      val r = Renaming(h.used)
      (r, h)
    case l =>
      val (r, newds) = canonize(h.dests, h.dests.map(_ => 0))
      (r, Hyperedge(l, r.inv comp h.source, newds))
  }
}

trait TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  override def allNodes: Set[Node] = nodes.toSet
  
  def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    val rinv = h.source.renaming.inv
    val sourcenode_used = rinv comp h.used
    
    val res =
      if(h.source.node.isInstanceOf[FreeNode]) {
        val n = newNode(sourcenode_used)
        h.source.node.gluedTo = n
        h.from(h.source.renaming comp n)
      }
      else {
        h
      }
        
    res.source.node.outsMut += res
    res.dests.foreach(_.node.insMut.add(res))
    reduceUsed(res.source.node, sourcenode_used)
    res
  }
  
  def addHyperedgeImpl(h_uncanonized: Hyperedge): RenamedNode = {
    val (ren, h) = canonize(h_uncanonized)
    require(h.source.node.isInstanceOf[FreeNode] || nodes(h.source.node))
    
    val pre_res = 
      if(h.dests.nonEmpty)
        h.dests(0).node.ins.find(x => x.label == h.label && x.dests == h.dests) match {
          case Some(x) if h.source.node == x.source.node => h.source
          case Some(x) => glueNodes(x.source, h.source)
          case None => 
            val newh = addHyperedgeSimple(h)
            checkIntegrity()
            onNewHyperedge(newh)
            newh.source
        }
      else
        nodes.find(_.outs.exists(_.label == h.label)) match {
          case Some(n) =>
            val src = n.outs.find(_.label == h.label).get.source
            glueNodes(src, h.source)
          case None => 
            val newh = addHyperedgeSimple(h)
            checkIntegrity()
            onNewHyperedge(newh)
            newh.source
        }
    
    ren comp pre_res
  }
  
  override def addHyperedge(hUnnorm: Hyperedge): RenamedNode = {
    val h = normalize(hUnnorm)
    val Hyperedge(l, src, ds) = h
    l match {
      case Id() =>
        // glue nodes if h is invertible
        val vec = ds(0).renaming.vector
        if(ds(0).used.size == ds(0).node.used.size) {
          glueNodes(src, ds(0))
        }
        else
          addHyperedgeImpl(h)
        src.deref
      case _ =>
        addHyperedgeImpl(h)
    }
  }
  
  override def newNode(used: Set[Int]): RenamedNode = {
    val n = new Node(used)
    nodes.add(n)
    n.deref
  }
  
  def removeNode(n: Node) {
    nodes -= n
    // we should leave n.outs and n.ins intact
    // so we remove all hyperedges incident with n
    // from all incident nodes except n
    for(h <- n.mins) {
      if(h.source.node != n)
        h.source.node.mouts -= h
      for(d <- h.dests if d.node != n)
        d.node.mins -= h
    }
    for(h <- n.mouts; d <- h.dests if d.node != n) {
      d.node.mins -= h
    }
  }
  
  def glueNodes(l1_underef: RenamedNode, r1_underef: RenamedNode): RenamedNode = {
    val (l1, r1) = (l1_underef.deref, r1_underef.deref)
    
    for((l1,r1) <- List((l1,r1), (r1,l1)))
      if(l1.node.isInstanceOf[FreeNode]) {
        assert(l1.node.gluedTo == null)
        assert(nodes.contains(r1.node))
        l1.node.gluedTo = l1.renaming.inv comp r1
        return r1
      }
    
    assert(nodes.contains(l1.node) && nodes.contains(r1.node))
    
    // TODO: Just for testing, should sort according to some simplicity measure
    //val List(l2,r2) = List(l1, r1).sortBy(_ => Random.nextBoolean())
    val List(l2,r2) = List(l1, r1).sortBy(x => (x.node.arity, -x.node.outs.size - x.node.ins.size))
    
    if(l2 != r2) {
      // We add temporary id hyperedges, so that HyperTester won't crash
      // This will also take care of arity reduction
      addHyperedgeSimple(canonize(Hyperedge(Id(), l2, List(r2)))._2)
      addHyperedgeSimple(canonize(Hyperedge(Id(), r2, List(l2)))._2)
      
      // Adding this hyperedges might trigger used set reduction
      // which in turn might perform node gluing
      val (l, r) = (l2.deref, r2.deref)
      
      // The node can be a renaming of itself, 
      // so we shouldn't do anything beyond adding hyperedges
      if(l.node == r.node)
        return l
      
      val l_renamed = r.renaming.inv comp l
      val r_node = r.node
      beforeGlue(l_renamed, r_node)
      
      checkIntegrity()
      removeNode(r_node)
      r_node.gluedTo = l_renamed
      
      // Readd hyperedges. This may lead to gluing of parent nodes. 
      for(h <- r_node.mouts)
        addHyperedgeSimple(canonize(normalize(h))._2)
      for(h <- r_node.mins)
        addHyperedgeSimple(canonize(normalize(h))._2)
      
      // Remove id hyperedges
      // Id endohyperedges are always redundant if they have id renamings
      l.node.outsMut -= canonize(normalize(Hyperedge(Id(), l.plain, List(l.plain))))._2
      l.node.insMut -= canonize(normalize(Hyperedge(Id(), l.plain, List(l.plain))))._2
        
      checkIntegrity()
      
      afterGlue(l.node)
      
      // maybe there appeared some more nodes to glue 
      glueParents(l.node)
      
      // Now l may be glued to something else
      l.deref
    }
    else //if(l == r)
      l2 // Nodes are already glued
  }
  
  // glue parents recursively
  def glueParents(n: Node) {
    val groups = n.ins.groupBy(h => (h.label, h.dests)).filter(_._2.size > 1)
    for((_, g) <- groups)
      g.toList.map(_.source).reduce(glueNodes)
  }
  
  def reduceUsed(node: Node, set: Set[Int]) {
    require(nodes(node))
    val newused = node.used & set
      
    if(newused != node.used) {
      node.mused = newused
      
      // TODO: I don't know whether we should call it here or after re-adding hyperedges 
      onUsedReduced(node)
      
      for(h <- node.ins ++ node.outs) {
        val nor = normalize(h)
        if(nor != h) {
          println("readd " + h)
          // h is not dereferenced, so we access its src/dst through nor
          nor.source.node.outsMut -= h
          nor.dests.map(_.node.insMut -= h)
          addHyperedge(nor)
        }
      }
    }
  }
  
  def nodeDotLabel(n: Node): String =
    n.uniqueName
  
  def toDot: String = {
    val sb = new StringBuilder()
    sb.append("digraph Hyper {\n")
    for(n <- nodes) {
      sb.append("\"" + n.uniqueName + "\"[label=\"" + nodeDotLabel(n) + "\", shape=box];\n")
      for(h <- n.outs) {
        def short(i: Int) = {
          val s = prettyRename(h.dests(i).renaming, h.dests(i).node.prettyDebug)
          if(s.size <= 4 && s != "")
            s.replaceAll("\\|", "\\\\|")
          else
            h.dests(i).node.uniqueName.dropWhile(_ != '@').tail.take(3)
        }
        
        val lab = "{" + h.source.renaming + "\\l" + h.label + "\\l|{" + 
            (0 until h.dests.length).map(i => "<" + i + ">" + short(i)).mkString("|") + "}}"
        sb.append("\"" + h.toString + "\"[label=\"" + lab + "\", shape=record];\n")
        sb.append("\"" + n.uniqueName + "\" -> \"" + h.toString + "\";\n")
        for((d,i) <- h.dests.zipWithIndex 
            if !d.node.outs.exists(h => h.label == Error() || h.label.isInstanceOf[Var]) ||
               d.node.prettyDebug == "") {
          sb.append("\"" + h.toString + "\":" + i + " -> \"" + d + "\";\n")
          sb.append("\"" + d + "\"[label=\"" + d.renaming + "\", shape=box];\n")
          sb.append("\"" + d + "\"" + " -> \"" + d.node.uniqueName + "\";\n")
        }
      }
      sb.append("\n")
    }
    sb.append("}\n")
    sb.toString
  }
  // TODO: Move this function somewhere, it was copypasted from Prettifier
  private def prettyRename(r: Renaming, orig: String): String = {
    "v([0-9]+)v".r.replaceAllIn(orig, 
          { m =>
              val i = m.group(1).toInt
              if(r(i) < 0)
                "_|_"
              else
                "v" + r(i) + "v" })
  }
  
  def integrityCheckEnabled = false
  def checkIntegrity() {
    if(integrityCheckEnabled)
      for(n <- nodes) {
        assert(n.deref.node == n)
        for(h <- n.ins) {
          //TODO: Not true, in rare cases there may be unuselesss id endohyperedges
          // but these cases should not be missed
          if(h.label.isInstanceOf[Id])
            assert(h.source.node != h.dests(0).node)
          assert(nodes(h.source.node))
          assert(h.dests.forall(n => nodes(n.node)))
          assert(h.source.node.outs(h))
          assert(h.dests.forall(_.node.ins(h)))
        }
        for(h <- n.outs) {
          assert(nodes(h.source.node))
          assert(h.dests.forall(n => nodes(n.node)))
          assert(h.dests.forall(_.node.ins(h)))
          assert(h.source.node == n)
        }
      }
  }
}


trait IntegrityCheckEnabled extends TheHypergraph {
  override def integrityCheckEnabled = true
}