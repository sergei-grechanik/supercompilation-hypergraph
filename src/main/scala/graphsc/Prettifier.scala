package graphsc

// Just supports named nodes
trait NamedNodes extends Hypergraph {
  val namedNodes = collection.mutable.Map[String, RenamedNode]()
  
  def apply(n: String): RenamedNode = namedNodes(n).deref
  
  def setName(n: Node, name: String) {
    namedNodes += name -> RenamedNode.fromNode(n)
  }
  
  def newNode(n: String, arity: Int): RenamedNode = 
    if(namedNodes.contains(n)) {
      namedNodes(n).deref
    }
    else {
      val node = newNode(0 until arity toSet)
      namedNodes += n -> node
      node.deref
    }
}

// Prettifies nodes on the fly
trait Prettifier extends TheHypergraph with NamedNodes {
  val prettyMap = collection.mutable.Map[Node, String]() 
  
  def pretty(n: Node): String = prettyMap.get(n) match {
    case None =>
      throw new NoSuchElementException("Node " + n + " is not pretty")
    case Some(s) => s
  }
  
  def pretty(n_underef: RenamedNode): String = {
    val RenamedNode(r, n) = n_underef.deref
    prettyRename(r, pretty(n))
  }
  
  def prettyUpdate(n: Node, s: String) {
    val old = prettyMap.get(n)
    if(old.isEmpty || old.get.size > s.size) {
      prettyMap(n) = s
      n.prettyDebug = s
      prettyUpdateIns(n)
    }
  }
  
  def prettyUpdateIns(n: Node) {
    for(h <- n.ins) {
      try {
        val s = prettyRename(h.source.renaming.inv, prettyHyperedge(h))
        prettyUpdate(h.source.node, s)
      } catch {
        case _: NoSuchElementException =>
      }
    }
  }
  
  override def setName(node: Node, name: String) {
    val n = node.deref.node
    super.setName(n, name)
    val newname = name + (0 until n.arity).map("v" + _ + "v").mkString(" ", " ", "")
    prettyUpdate(n, newname)
  }
  
  override def newNode(n: String, arity: Int): RenamedNode = {
    val node = super.newNode(n, arity)
    prettyUpdate(node.node, n + (0 until arity).map("v" + _ + "v").mkString(" ", " ", ""))
    node
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    try {
      val s = prettyRename(h.source.renaming.inv, prettyHyperedge(h))
      prettyUpdate(h.source.node, s)
    } catch {
      case _: NoSuchElementException =>
    }
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    val lp = prettyMap.get(l.node)
    val rp = prettyMap.get(r).map(prettyRename(l.renaming.inv, _))
    if(rp != None) {
      prettyUpdate(l.node, rp.get)
    }
    super.beforeGlue(l, r)
  }
  
  override def afterGlue(n: Node) {
    prettyUpdateIns(n)
    super.afterGlue(n)
  }
  
  private def indent(s: String, ind: String = "  "): String = "  " + indent1(s, ind)
  private def indent1(s: String, ind: String = "  "): String = s.replace("\n", "\n" + ind)
  
  def prettyHyperedge(h: Hyperedge, prettyfun: RenamedNode => String = pretty _): String = 
    h.label match {
      case Construct(name) => name + h.dests.map("(" + prettyfun(_) + ")").mkString(" ", " ", "")
      case CaseOf(cases) =>
        "case " + prettyfun(h.dests(0)) + " of {\n" +
        indent((
          for(((n,k),e) <- cases zip h.dests.tail) yield
            n + " " + (0 until k map (i => "c" + i + "c")).mkString(" ") + " -> " +
            indent1(prettyUnshift(k, prettyfun(e)))
        ).mkString(";\n")) + "\n}"
      case Let() =>
        val args = h.dests.tail.map(prettyfun(_))
        val vars = args.zipWithIndex.map {
          case (e,i) => "b" + i + "b = " + indent1(e, "      ")
        }
        val in = indent1(prettyfun(h.dests(0)), "   ")
        
        val letres =
          "let\n" + indent(vars.mkString(";\n"), "  ") + "\nin " + 
              in.replaceAll("v([0-9]+)v", "b$1b")
        
        lazy val callres =
          "v([0-9]+)v".r.replaceAllIn(in, m => "(" + args(m.group(1).toInt) + ")" )
            
        if(args.forall(!_.contains("\n")) && callres.size < letres.size)
          callres
        else
          letres
      case Var() => "v" + 0 + "v"
      case Id() => prettyfun(h.dests(0))
      case Tick() => "* " + prettyfun(h.dests(0))
      case Improvement() => ">= " + prettyfun(h.dests(0))
      case Unused() => "_|_"
    }
  
  def prettyRename(r: Renaming, orig: String): String = {
    "v([0-9]+)v".r.replaceAllIn(orig, 
          { m =>
              val i = m.group(1).toInt
              if(r(i) < 0)
                "_|_"
              else
                "v" + r(i) + "v" })
  }
  
  def prettyUnshift(sh: Int, orig: String): String = {
    "v([0-9]+)v".r.replaceAllIn(orig, 
          { m =>
              val i = m.group(1).toInt
              if(i < sh)
                "c" + i + "c"
              else
                "v" + (i - sh) + "v" })
  }
                
  def prettyTwoHyperedges(h1: Hyperedge, h2: Hyperedge): String = {
    prettyHyperedge(h1, 
        (n =>
          if(n.node == h2.source.node)
            prettyRename(n.renaming comp h2.source.renaming.inv,
                prettyHyperedge(h2, (n => "{" + pretty(n) + "}")))
          else
            "{" + pretty(n) + "}"))
  }
  
  def prettyProgram: String = {
    (for(rnode:RenamedNode <- namedNodes.values.toSet; h <- rnode.deref.node.outs.iterator) yield {
      pretty(rnode.deref.node) + " = " + prettyHyperedge(h) + ";\n"
    }).toList.reduce(_ + _)
  }
  
  def nameUnnamed() {
    var i = 0
    for(n <- nodes if n.ins.size > 1) {
      if(!prettyMap.contains(n)) {
        var newname: String = null
        do {
          newname = "f_" + i
          i += 1
        } while(namedNodes.contains(newname))
        
        setName(n, newname)
      }
    }
  }
  
  override def nodeDotLabel(n: Node): String =
    super.nodeDotLabel(n) + "\\l" +
    pretty(n).replace("\n", "\\l") + "\\l" +
    "\\l"
    
  def statistics() {
    val hyperedges = allNodes.toList.flatMap(n => n.ins ++ n.outs).toSet
    val mostgen = allNodes.maxBy(_.used.size)
    val biggest = pretty(allNodes.maxBy(pretty(_).size))
    println("Nodes: " + allNodes.size)
    println("Hyperedges: " + hyperedges.size)
    println("Largest arity: " + mostgen.used.size + "\n" + mostgen + "\n" + pretty(mostgen) + "\n")
    println("biggest prettified representation:\n" + biggest + "\n")
  }
}

