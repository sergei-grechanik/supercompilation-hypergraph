package graphsc

// Just supports named nodes
trait NamedNodes extends Hypergraph {
  val namedNodes = collection.mutable.Map[String, Node]()
  
  def apply(n: String): Node = namedNodes(n).realNode
  
  def newNode(n: String, arity: Int): Node = 
    if(namedNodes.contains(n)) {
      namedNodes(n)
    }
    else {
      val node = newNode(arity)
      namedNodes += n -> node
      node
    }
}

// Prettifies nodes on the fly
trait Prettifier extends TheHypergraph with NamedNodes {
  val prettyMap = collection.mutable.Map[Node, String]() 
  
  def pretty(n: Node): String = prettyMap.get(n.realNode) match {
    case None =>
      throw new NoSuchElementException("Node " + n + " is not pretty")
    case Some(s) => s
  }
  
  def prettySet(n: Node, s: String) {
    prettyMap += n.realNode -> s
    n.realNode.prettyDebug = s
  }
  
  override def newNode(n: String, arity: Int): Node = {
    val node = super.newNode(n, arity)
    prettySet(node.realNode, n + (0 until arity).map("v" + _ + "v").mkString(" ", " ", ""))
    node
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    try {
      val s = prettyHyperedge(h)
      prettyMap.get(h.source.realNode) match {
        case Some(p) if p.length <= s.length =>
        case _ =>
          prettySet(h.source.realNode, s)
      }
      super.onNewHyperedge(h)
    } catch {
      case _: NoSuchElementException =>
    }
  }
  
  override def beforeGlue(l1: Node, r1: Node) {
    val l = l1.realNode
    val r = r1.realNode
    val lp = prettyMap.get(l)
    val rp = prettyMap.get(r)
    assert(lp != None || rp != None)
    if(lp != None && (rp == None || lp.get.length <= rp.get.length)) {
      prettySet(r, lp.get)
    } else {
      prettySet(l, rp.get)
    }
    super.beforeGlue(l1, r1)
  }
  
  private def indent(s: String, ind: String = "  "): String = "  " + indent1(s, ind)
  private def indent1(s: String, ind: String = "  "): String = s.replace("\n", "\n" + ind)
  
  def prettyHyperedge(h: Hyperedge, prettyfun: Node => String = pretty _): String = h.label match {
    case Construct(name) => name + " " + h.dests.map("(" + prettyfun(_) + ")").mkString(" ")
    case CaseOf(cases) =>
      "case " + prettyfun(h.dests(0)) + " of {\n" +
      indent((
        for(((n,k),e) <- cases zip h.dests.tail) yield
          n + " " + (0 until k map (i => "b" + (i + e.arity - k) + "b")).mkString(" ") + " -> " +
          indent1(prettyfun(e).replaceAll("v([0-9]+)v", "b$1b"))
      ).mkString(";\n")) + "\n}"
    case Let() =>
      val vars = h.dests.tail.zipWithIndex.map {
        case (e,i) => "b" + i + "b = " + indent1(prettyfun(e), "      ")
      }
      val in = indent1(prettyfun(h.dests(0)), "   ")
      "let\n" + indent(vars.mkString(";\n"), "  ") + "\nin " + 
      in.replaceAll("v([0-9]+)v", "b$1b")
    case Var(i) => "v" + i + "v"
    case Id() => prettyfun(h.dests(0))
    case Tick() => "* " + prettyfun(h.dests(0))
    case Improvement() => ">= " + prettyfun(h.dests(0))
    case Renaming(vec) =>
      val orig = prettyfun(h.dests(0))
      "v([0-9]+)v".r.replaceAllIn(orig, 
          m => "v" + vec(m.group(1).toInt) + "v")
    case Error() => "_[fail]_"
  }
  
  def prettyTwoHyperedges(h1: Hyperedge, h2: Hyperedge): String = {
    prettyHyperedge(h1, 
        (n =>
          if(n == h2.source)
            "[" + prettyHyperedge(h2, (n => "{" + pretty(n) + "}")) + "]"
          else
            "{" + pretty(n) + "}"))
  }
  
  override def nodeDotLabel(n: Node): String =
    super.nodeDotLabel(n) + "\\l" +
    pretty(n).replace("\n", "\\l") + "\\l" +
    "\\l"
    
  def statistics() {
    val nodes = allNodes.toList.map(n => n.arity + "\n" + pretty(n))
    val mostpop = nodes.groupBy(identity).mapValues(_.size).maxBy(_._2)
    println("Nodes: " + allNodes.size + " should be: " + nodes.distinct.size)
    println("Most popular(" + mostpop._2 + "): arity " + mostpop._1 + "\n")
  }
}

