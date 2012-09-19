package graphsc

import scala.util.parsing.combinator._

class ExprParser(graph: NamedNodes) extends JavaTokenParsers {
  def apply(s: String) {
    val success = parseAll(prog, s).successful
    assert(success) // We've modified the graph even if the parsing wasn't successful
    // it is better to rewrite it in a bit more functional style
  }
  
  def prog: Parser[Any] = repsep(definition, ";") ~ opt(";")
  
  def definition: Parser[RenamedNode] = 
    (sign <~ "=") ~! expr ^^
    { case (n,table)~e => graph.add(Id(), n, List(e(table))) }
  
  def sign: Parser[(RenamedNode, Map[String,Int])] =
    fname ~ rep(fname) ^^
    { case i~vs => (graph.newNode(i, vs.length.toInt), vs.zipWithIndex.toMap) }
  
  def fname = "[a-z][a-zA-Z0-9.@_]*".r
  def cname = "[A-Z][a-zA-Z0-9.@_]*".r
  
  private def theVar(v: Int): RenamedNode = graph.variable(v)
  
  def onecase: Parser[Map[String,Int] => ((String, Int), RenamedNode)] =
    cname ~ rep(fname) ~ "->" ~ expr ^^
    {case n~l~"->"~e => table =>
      val lsize = l.size
      val newtable = table.mapValues(_ + lsize) ++ (l zip (0 until lsize))
      ((n, lsize), e(newtable))}
  
  def caseof: Parser[Map[String,Int] => RenamedNode] =
    ("case" ~> argexpr <~ "of") ~! ("{" ~> repsep(onecase, ";") <~ "}") ^^
    { case e~lst => table =>
        val cases = lst.map(_(table))
        graph.add(CaseOf(cases.map(_._1)), e(table) :: cases.map(_._2)) }
  
  def call: Parser[Map[String,Int] => RenamedNode] =
    fname ~ rep(argexpr) ^^
    { case f~as => table =>
        if(as.isEmpty && table.contains(f))
          theVar(table(f))
        else {
          val fun = graph.newNode(f, as.length).deref
          graph.add(Let(), fun :: as.map(_(table)))
        }
    }
  
  def variable: Parser[Map[String,Int] => RenamedNode] =
    fname ^^
    { case f => table => theVar(table(f))}
    
  def cons: Parser[Map[String,Int] => RenamedNode] =
    cname ~ rep1(argexpr) ^^
    { case c~as => table =>
        graph.add(Construct(c), as.map(_(table))) }
  
  def zeroargCons: Parser[Map[String,Int] => RenamedNode] =
    cname ^^
    { case c => table =>
        graph.add(Let(), List(
            graph.add(Construct(c), List()))) }
  
  def expr: Parser[Map[String,Int] => RenamedNode] =
    caseof |
    "(" ~> expr <~ ")" |
    call |
    cons |
    zeroargCons
  
  def argexpr: Parser[Map[String,Int] => RenamedNode] =
    variable |
    caseof |
    zeroargCons |
    "(" ~> expr <~ ")"
    
}
