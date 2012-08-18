package graphsc

import scala.util.parsing.combinator._

class ExprParser(graph: NamedNodes) extends JavaTokenParsers {
  def apply(s: String) {
    val success = parseAll(prog, s).successful
    assert(success) // We've modified the graph even if the parsing wasn't successful
    // it is better to rewrite it in a bit more functional style
  }
  
  def prog: Parser[Any] = repsep(definition, ";") ~ opt(";")
  
  def definition: Parser[Node] = 
    (sign <~ "=") ~! expr ^^
    { case (n,table)~e => graph.glueNodes(n, e(table)) }
  
  def sign: Parser[(Node, Map[String,Int])] =
    fname ~ rep(fname) ^^
    { case i~vs => (graph.addNode(i, vs.length.toInt), vs.zipWithIndex.toMap) }
  
  def fname = "[a-z][a-zA-Z0-9.@_]*".r
  def cname = "[A-Z][a-zA-Z0-9.@_]*".r
  
  private def theVar(a: Int, v: Int): Node = graph.addNode(Var(a, v), List())
  
  private def tableArity(table: Map[String, Int]): Int =
    if(table.isEmpty)
      0
    else
      table.values.max + 1
  
  def onecase: Parser[Map[String,Int] => ((String, Int), Node)] =
    cname ~ rep(fname) ~ "->" ~ expr ^^
    {case n~l~"->"~e => table =>
      val lsize = l.size
      val ar = tableArity(table)
      val newtable = table ++ (l zip (0 until lsize).map(_ + ar))
      ((n, lsize), e(newtable))}
  
  def caseof: Parser[Map[String,Int] => Node] =
    ("case" ~> argexpr <~ "of") ~! ("{" ~> repsep(onecase, ";") <~ "}") ^^
    { case e~lst => table =>
        val cases = lst.map(_(table))
        graph.addNode(CaseOf(cases.map(_._1)), e(table) :: cases.map(_._2)) }
  
  def call: Parser[Map[String,Int] => Node] =
    fname ~ rep(argexpr) ^^
    { case f~as => table =>
        if(as.isEmpty && table.contains(f))
          theVar(tableArity(table), table(f))
        else {
          val fun = graph.addNode(f, as.length)
          graph.addNode(Let(tableArity(table)), fun :: as.map(_(table)))
        }
    }
  
  def variable: Parser[Map[String,Int] => Node] =
    fname ^^
    { case f => table => theVar(tableArity(table), table(f))}
    
  def cons: Parser[Map[String,Int] => Node] =
    cname ~ rep1(argexpr) ^^
    { case c~as => table =>
        graph.addNode(Construct(c), as.map(_(table))) }
  
  def zeroargCons: Parser[Map[String,Int] => Node] =
    cname ^^
    { case c => table =>
        graph.addNode(Let(tableArity(table)), List(
            graph.addNode(Construct(c), List()))) }
  
  def expr: Parser[Map[String,Int] => Node] =
    caseof |
    "(" ~> expr <~ ")" |
    call |
    cons |
    zeroargCons
  
  def argexpr: Parser[Map[String,Int] => Node] =
    variable |
    caseof |
    zeroargCons |
    "(" ~> expr <~ ")"
    
}

class TooManyNodesException(s: String) extends Exception(s)

trait Visualizer extends TheHypergraph {
  val dotObjects = collection.mutable.Map[String, String]()
  
  var frames: List[Set[String]] = Nil
  
  def save(e: Any = null) {
    frames ::= dotVisible
  }
  
  def writeDotFrames() {
    for((s,i) <- frames.reverse.zipWithIndex) {
      val out = new java.io.FileWriter("test" + i + ".dot")
      out.write(dotVisualize(s))
      out.close
    }
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    dotHyperedge(h)
    save()
    super.onNewHyperedge(h)
  }
  
  override def afterGlue(n: Node) {
    for(h <- n.ins ++ n.outs)
      dotHyperedge(h)
    save()
    super.afterGlue(n)
  }
  
  def dotHyperedge(h: Hyperedge) {
    dotObjects += ("\"" + h.source.uniqueName + "\"") -> "label=\"\",shape=circle"
    dotObjects += ("\"" + h + "\"") -> 
      ("shape=record,label=\"{" + h.label.toString + "|{" + 
            (0 until h.dests.length).map("<" + _ + ">").mkString("|") + "}}\"")
    dotObjects += ("\"" + h.source.uniqueName + "\" -> \"" + h + "\"") -> "label=\"\""
    for((d,i) <- h.dests.zipWithIndex)
          dotObjects += ("\"" + h + "\":" + i + " -> \"" + d.uniqueName + "\"") -> "label=\"\""
  }
  
  def dotVisible: Set[String] = {
    val s = collection.mutable.Set[String]()
    for(n <- nodes) {
      s += "\"" + n.uniqueName + "\""
      for(h <- n.ins ++ n.outs) {
        s += "\"" + h + "\""
        s += "\"" + h.source.uniqueName + "\" -> \"" + h + "\""
        for((d,i) <- h.dests.zipWithIndex)
          s += "\"" + h + "\":" + i + " -> \"" + d.uniqueName + "\""
      }
    }
    s.toSet
  }
  
  def dotVisualize(objs: Set[String]): String = {
    val s = new StringBuilder
    s.append("digraph Hyper {\n")
    for((n,p) <- dotObjects) {
      val c =
        if(objs(n))
          ",color=black"
        else
          ",color=white,fontcolor=white"
      s.append(n + "[" + p + c + "];\n")
    }
    s.append("}")
    s.toString
  }
}

trait HyperLogger extends Hypergraph {
  override def onNewHyperedge(h: Hyperedge) {
    println("new " + h)
    super.onNewHyperedge(h)
  }
  
  /*override def onNewNode(n: Node) {
    println("new node " + n.uniqueName)
    super.onNewNode(n)
  }*/
  
  override def beforeGlue(l: Node, r: Node) {
    println("glue " + l.uniqueName + " " + r.uniqueName)
    super.beforeGlue(l, r)
  }
}

trait Transformer extends HyperTester {
  val updatedHyperedges = collection.mutable.Set[Hyperedge]()
  
  var maxar = 0
  override def onNewHyperedge(h: Hyperedge) {
    maxar = maxar max h.arity
    println("### " + h.arity + " ############# max: " + maxar)
    updatedHyperedges += h
    super.onNewHyperedge(h)
  }
  
  override def afterGlue(n: Node) {
    updatedHyperedges ++= n.outs
    super.afterGlue(n)
  }
  
  def transforming(hs: Hyperedge*) {
    statistics()
    println("transforming:")
    for(h <- hs)
      println("    " + h)
  }
  
  def transform(
        h: Hyperedge, 
        t: PartialFunction[Hyperedge, List[Hyperedge]], 
        name: String) {
    t.lift(h) match {
      case Some(l) =>
        println(name)
        transforming(h)
        for(nh <- l)
          addHyperedge(nh)
      case None =>
    }
  }
  
  def transform(
        h: Hyperedge, 
        t: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]],
        name: String)
  (implicit dummy1: DummyImplicit) {
    for(d <- h.dests; h1 <- d.outs) {
      t.lift(h, h1) match {
        case Some(l) =>
          println(name)
          transforming(h, h1)
          for(nh <- l)
            addHyperedge(nh)
        case None =>
      }
    }
  }
  
  def transform(h: Hyperedge) {
    import Transformations._
    val tr = List(
        "varToRenaming" -> varToRenaming,
        "letToRenaming" -> letToRenaming)
        
    for((name,trans) <- tr) {
      if(trans.isDefinedAt(h)) {
        println(name)
        transforming(h)
        for(nh <- trans(h))
          addHyperedge(nh)
      }
    }
    
    for(nh <- throughRenamings(h)) {
      println("throughRenamings")
      transforming(h)
      addHyperedge(nh)
    }
  }
  
  def transform(h1: Hyperedge, h2: Hyperedge) {
    import Transformations._
    val tr = List(
        "letVar" -> letVar,
        "letLet" -> letLet,
        "letCaseOf" -> letCaseOf,
        "letOther" -> letOther,
        "caseReduce" -> caseReduce,
        "caseVar" -> caseVar,
        "caseCase" -> caseCase,
        "letRenaming" -> letRenaming,
        "renamingRenaming" -> renamingRenaming,
        "otherRenaming" -> otherRenaming)
        
    for((name,trans) <- tr) {
      if(trans.isDefinedAt((h1,h2))) {
        println(name)
        transforming(h1, h2)
        for(nh <- trans((h1,h2)))
          addHyperedge(nh)
      }
    }
    
    for(nh <- throughRenamings(h1)) {
      println("throughRenamings")
      transforming(h1)
      addHyperedge(nh)
    }
  }
  
  def transform() {
    import Transformations._
    
    //if(nodes.size > 10000)
    //  throw new TooManyNodesException("")
    
    val set = updatedHyperedges.map(_.derefGlued)
    println("***********************")
    println("*** updnhyp: " + set.size)
    println("***********************")
    updatedHyperedges.clear()
    for(h <- set if h.derefGlued == h) {
      transform(h)
      for(d <- h.dests; h1 <- d.outs)
        transform(h, h1)
      for(h1 <- h.source.ins)
        transform(h1, h)
    }
  }
}

trait Canonizer extends TheHypergraph {
  override def preprocessors = canonize _ :: super.preprocessors
  def canonize(h: Hyperedge): List[Hyperedge] = {
    Transformations.throughRenamings(h) match {
      case Nil =>
        println("uncanonized " + h)
        //Transformations.throughRenamings(h)       
        List(h)
      case lst => lst 
    }
  }
}

object Test {
  def main(args: Array[String]) {
    val g = new TheHypergraph
        with Canonizer
        with NamedNodes
        with Transformer
        //with Visualizer 
        //with HyperTester
        //with HyperLogger 
    
    implicit def peano(i: Int): Value =
      if(i == 0)
        Value("Z", List())
      else
        Value("S", List(peano(i-1)))
        
    def list(vs: Value*): Value = 
      (vs :\ Value("N", List()))((x, y) => Value("C", List(x, y)))
    
    val p = new ExprParser(g)
    p("add x y = case x of { Z -> y; S x -> S (add x y) }")
    assert(g.runNode(g("add"), Vector(2, 3)) == peano(5))
    /*p("mul x y = case x of { Z -> Z; S x -> add y (mul x y) }")
    assert(g.runNode(g("mul"), Vector(2, 3)) == peano(6))
    p("padd x y = case x of { Z -> y; S x -> S (padd y x) }")
    assert(g.runNode(g("padd"), Vector(2, 3)) == peano(5))
    p("pmul x y = case x of { Z -> Z; S x -> padd y (pmul y x) }")
    assert(g.runNode(g("pmul"), Vector(2, 3)) == peano(6))
    p("id x = case x of {Z -> Z; S x -> S (id x)}")
    assert(g.runNode(g("id"), Vector(3)) == peano(3))
    p("nrev x = case x of {Z -> Z; S x -> add (nrev x) (S Z)}")
    assert(g.runNode(g("nrev"), Vector(2)) == peano(2))
    p("fac x = case x of {Z -> S Z; S x -> mul (S x) (fac x)}")
    assert(g.runNode(g("fac"), Vector(4)) == peano(24))
    p("fib x = case x of {Z -> Z; S x -> case x of {Z -> S Z; S x -> add (fib (S x)) (fib x)}}")
    assert(g.runNode(g("fib"), Vector(6)) == peano(8))
    p("append x y = case x of {N -> y; C a x -> C a (append x y)}")
    p("nrevL x = case x of {N -> N; C a x -> append (nrevL x) (C a N)}")
    assert(g.runNode(g("nrevL"), Vector(list(1,2,3,4))) == list(4,3,2,1))*/
    try {
    for(i <- 0 to 50) {
      println("nodes: " + g.allNodes.size)
      g.transform()
    }
    }catch {case _:TooManyNodesException => println("aborted")}
    
    println("**********************************************")
    //g.writeDotFrames
    
    val out = new java.io.FileWriter("test.dot")
    out.write(g.toDot)
    out.close
  }
}