package graphsc

case class ProgramSubgraph(nodes: Set[Node], hyperedges: Map[Node, Hyperedge])

// Convert a hypergraph to a program
class ToProgram(namegen: NameGenerator[Node] = new NameGenerator) {
  
  def apply(graph: Hypergraph, ns: Iterable[(String, RenamedNode)], ps: ProgramSubgraph): Program =
    apply(graph, ns, h => ps.hyperedges.get(h.source.deref.node).exists(_ == h))
  
  def apply(graph: Hypergraph, ns: Iterable[(String, RenamedNode)], 
            include: Hyperedge => Boolean = _ => true): Program = {
    var varnum = 0
    def newvar() = {
      varnum += 1
      "b_" + varnum
    }
    
    val nodes_to_convert = ns.map(_._2.deref.node).toSet
    
    val node2name = collection.mutable.Map[Node, (Renaming, String)]()
    val name2defs = collection.mutable.Map[String, List[Expr]]()
    
    def node_name(n: RenamedNode): (Renaming, String) = graph match {
      case _ if ns.exists(_._2 ~~ n) =>
        val (name, n1) = ns.find(_._2 ~~ n).get
        (n.renaming comp n1.renaming.inv, name)
      case nn:NamedNodes if nn.namedNodes.values.exists(_ ~~ n) =>
        val (name, n1) = nn.namedNodes.find(_._2 ~~ n).get
        (n.renaming comp n1.renaming.inv, name)
      case p:Prettifier if(p.prettyMap.contains(n.node)) =>
        (n.renaming, "f_" + p.nameGen(p.pretty(n.node)))
      case _ => (n.renaming, "n_" + namegen(n.node))
    }
    
    def hyperedge_to_expr(h: Hyperedge): Expr = h.label match {
      case Construct(name) => ExprCall(ExprConstr(name), h.dests.map(go(_)))
      case CaseOf(cases) =>
        val newcases = 
          for(((c,k), d) <- cases zip h.dests.tail) yield {
            val bvars = (0 until k).toList.map(_ => newvar())
            (c, bvars, renameExprForCaseOf(bvars, go(d)))
          }
        ExprCaseOf(go(h.dests(0)), newcases)
      case Let() =>
        go(h.dests(0)) match {
//          case ExprCall(name, vs) if vs.forall(_.isInstanceOf[ExprVar]) && 
//                                     vs.distinct.size == vs.size =>
//            ExprCall(name, )
          case expr =>
            val binds = h.dests.tail.map(d => (newvar(), go(d)))
            ExprLet(renameExprForCaseOf(binds.map(_._1), expr), binds)
        }
      case Id() => go(h.dests(0))
      case Tick() => go(h.dests(0))
      case Improvement() => go(h.dests(0))
      case Var() => ExprVar("v_0")
      case Unused() => ExprVar("undefined")
    }
    
    def go(n: RenamedNode): Expr = node2name.get(n.node) match {
      case Some((ren, name)) =>
        if(n.arity == 0) ExprFun(name)
        else renameExpr(n.renaming comp ren, 
              ExprCall(ExprFun(name), 
                  (0 until (ren.inv comp n.node).arity).toList.map(i => ExprVar("v_" + i))))
      case None =>
        val hs = n.node.outs.filter(include)
        assert(hs.nonEmpty)
        if(hs.size > 1 || 
           ((n.node.ins.filter(include).size > 1 || nodes_to_convert(n.node)) && 
               !trivial(hs(0)))) {
          val name = node_name(n.node.deref)
          node2name += n.node -> name
          name2defs += name._2 -> 
            hs.map(h =>
              closeExpr((name._1.inv comp n.node).arity,
                renameExpr(name._1.inv comp h.source.renaming.inv, 
                    hyperedge_to_expr(h))))
          go(n)
        } else
          renameExpr(n.renaming comp hs(0).source.renaming.inv, hyperedge_to_expr(hs(0)))
    }
    
    for((name,rnode1) <- ns if !name2defs.contains(name)) {
      val rnode = rnode1.deref
      val expr = go(rnode)
      // The function "name" may be added during the execution of go
      if(!name2defs.contains(name))
        name2defs += name -> List(closeExpr(rnode.arity, expr))
    }
    
    Program(name2defs.toMap)
  }
  
  def renameExprForCaseOf(bs: List[String], e: Expr): Expr = {
    e.renameFreeVarsUnsafe1 { v =>
      lazy val num = v.substring(2)
      if(v.startsWith("v_") && num.forall(_.isDigit)) {
        val i = num.toInt
        if(i < bs.size) bs(i)
        else "v_" + (i - bs.size)
      } else v
    }
  }
  
  def renameExpr(r: Renaming, e: Expr): Expr = {
    e.renameFreeVarsUnsafe1 { v =>
      lazy val num = v.substring(2)
      if(v.startsWith("v_") && num.forall(_.isDigit)) {
        val newnum = r(num.toInt)
        if(newnum != -1)
          "v_" + newnum
        else
          "undefined"
      } else v
    }
  }
  
  def closeExpr(arity: Int, e: Expr): Expr = {
    if(arity == 0) e
    else ExprLambda((0 until arity).toList.map("v_" + _), e)
  }
  
  def trivial(h: Hyperedge): Boolean = h.label match {
    case Var() => true
    case Unused() => true
    case Construct(_) if h.dests.isEmpty => true
    case _ => false
  }
}

object ToProgram extends ToProgram(new NameGenerator) {

}