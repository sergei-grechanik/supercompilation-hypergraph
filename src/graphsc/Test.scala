package graphsc

import scala.util.parsing.combinator._

class ExprParser(graph: NamedNodes) extends JavaTokenParsers {
  def apply(s: String) {
    val success = parseAll(prog, s).successful
    assert(success) // We've modified the graph even if the parsing wasn't successful
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
  
  private lazy val theVar = graph.addHyperedge(Hyperedge(Var(), null, List())).source
  def variable: Parser[Node] =
    wholeNumber ^^
    { case i => 
        graph.addHyperedge(
            Hyperedge(new Renaming(0 -> i.toInt), null, List(theVar))).source }
  
  def onecase: Parser[((String, List[Int]), Node)] =
    cname ~ rep(wholeNumber) ~ "->" ~ expr ^^
    {case n~l~"->"~e => ((n, l.map(_.toInt)), e)}
  
  def caseof: Parser[Node] =
    ("case" ~> expr <~ "of") ~ ("{" ~> repsep(onecase, ";") <~ "}") ^^
    { case e~lst => 
        graph.addHyperedge(
            Hyperedge(CaseOf(lst.map(_._1)), null, e :: lst.map(_._2))).source }
  
  def call: Parser[Node] =
    fname ~ rep(argexpr) ^^
    { case f~as =>
        var res = graph.addNode(f, as.length)
        for((a,i) <- as.zipWithIndex)
          res = graph.addHyperedge(Hyperedge(Let(i), null, List(res, a))).source
        res
    }
    
  def cons: Parser[Node] =
    cname ~ rep(argexpr) ^^
    { case c~as =>
        graph.addHyperedge(Hyperedge(Construct(c), null, as)).source }
  
  def expr: Parser[Node] =
    argexpr |
    call |
    cons
  
  def argexpr: Parser[Node] =
    variable |
    caseof |
    "(" ~> expr <~ ")"
    
}

object Test {
  def main(args: Array[String]) {
    val g = new TheHypergraph with NamedNodes with HyperTester
    val p = new ExprParser(g)
    p("add/2 = case 0 of { Z -> 1; S 0 -> S (add 0 1) }")
    
    val zero = Value("Z", List())
    val one = Value("S", List(zero))
    val two = Value("S", List(one))
    val three = Value("S", List(two))
    
    println(g.runNode(g("add"), List(two, three)))
    
    for(i <- 0 to 5) {
      println("nodes: " + g.nodes.size)
      for(n <- g.nodes; h <- n.outs) {
        Transformations.caseCase(g, h)
        Transformations.caseReduce(g, h)
        Transformations.letDown(g, h)
        Transformations.propagate(g, h)
        Transformations.throughRenaming(g, h)
      }
    }
    
    println(g.toDot)
  }
}