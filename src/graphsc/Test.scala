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
    (sign <~ "=") ~ expr ^^
    { case n~e => graph.glueNodes(n, e) }
  
  def sign: Parser[Node] =
    (fname <~ "/") ~ wholeNumber ^^
    { case i~a => graph.addNode(i, a.toInt) }
  
  def fname = "[a-z][a-zA-Z0-9.@_]*".r
  def cname = "[A-Z][a-zA-Z0-9.@_]*".r
  
  private lazy val theVar = graph.addNode(Var(), List())
  def variable: Parser[Node] =
    wholeNumber ^^
    { case i => 
        graph.addNode(Renaming(0 -> i.toInt), List(theVar)) }
  
  def onecase: Parser[((String, List[Int]), Node)] =
    cname ~ rep(wholeNumber) ~ "->" ~ expr ^^
    {case n~l~"->"~e => ((n, l.map(_.toInt)), e)}
  
  def caseof: Parser[Node] =
    ("case" ~> expr <~ "of") ~ ("{" ~> repsep(onecase, ";") <~ "}") ^^
    { case e~lst => 
        graph.addNode(CaseOf(lst.map(_._1)), e :: lst.map(_._2)) }
  
  def call: Parser[Node] =
    fname ~ rep(argexpr) ^^
    { case f~as =>
        val fun = graph.addNode(f, as.length)
        graph.addNode(Let(0 until as.length toList), fun :: as)
    }
    
  def cons: Parser[Node] =
    cname ~ rep(argexpr) ^^
    { case c~as =>
        graph.addNode(Construct(c), as) }
  
  def expr: Parser[Node] =
    argexpr |
    call |
    cons
  
  def argexpr: Parser[Node] =
    variable |
    caseof |
    "(" ~> expr <~ ")"
    
}

class TooManyNodesException(s: String = "") extends Exception(s)

trait Visualizer extends TheHypergraph {
  val dotObjects = collection.mutable.Map[String, String]()
  
  var frames: List[Set[String]] = Nil
  
  var cnt = 0
  def save(e: Any = null) {
    cnt += 1
    if(cnt > 300)
      throw new TooManyNodesException
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
  override def transforming(hs: Hyperedge*) {
    println("transforming:")
    for(h <- hs)
      println("    " + h)
  }
  
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

trait Transformer extends TheHypergraph with HyperTester with Transformations {
  val updatedNodes = collection.mutable.Set[Node]()
  
  override def onNewHyperedge(h: Hyperedge) {
    updatedNodes += h.source
    super.onNewHyperedge(h)
  }
  
  override def afterGlue(n: Node) {
    updatedNodes += n
    super.afterGlue(n)
  }
  
  def transform() {
    val set = updatedNodes.map(n => n.outs ++ n.ins).flatten
    val g = this
    println("***********************")
    println("*** updnodes: " + updatedNodes.size + " hyp: " + set.size)
    println("***********************")
    updatedNodes.clear()
    for(h <- set) {
        /*println("letdown")
        Transformations.letDown(g, h)
        Transformations.glueAll(g)
        g.statistics()
        println("reverseRenaming")
        Transformations.reverseRenaming(g, h)
        Transformations.glueAll(g)
        g.statistics()
        println("throughRenaming")
        Transformations.throughRenaming(g, h)
        Transformations.glueAll(g)
        g.statistics()
        println("casecase")
        Transformations.caseCase(g, h)
        Transformations.glueAll(g)
        g.statistics()
        println("casereduce")
        Transformations.caseReduce(g, h)
        Transformations.glueAll(g)
        g.statistics()
        println("propagate")
        Transformations.propagate(g, h)
        Transformations.glueAll(g)
        g.statistics()*/
    }
  }
}

object Test {
  def main(args: Array[String]) {
    val g = new TheHypergraph 
        with NamedNodes
        with Transformer 
        with Visualizer 
        with HyperTester
        with HyperLogger 
    
    val zero = Value("Z", List())
    val one = Value("S", List(zero))
    val two = Value("S", List(one))
    val three = Value("S", List(two))
    
    val p = new ExprParser(g)
    p("add/2 = case 0 of { Z -> 1; S 0 -> S (add 0 2) }")
    println(g.runNode(g("add"), Vector(two, three)))
    p("mul/2 = case 0 of { Z -> Z; S 0 -> add 2 (mul 0 2) }")
    println(g.runNode(g("mul"), Vector(two, three)))
    //p("z/1 = case 0 of {Z -> Z; S 0 -> S (z 0)}")
    //println(g.runNode(g("z"), Vector(two)))
    try {
    for(i <- 0 to 50) {
      println("nodes: " + g.nodes.size)
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