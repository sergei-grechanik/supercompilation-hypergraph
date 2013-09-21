package graphsc

import graphsc.residualization._

trait GoalChecker extends Hypergraph {
  def checkGoal(g: GoalProp): Boolean = g match {
    case GoalPropEqModuloRen(l, r) => l ~~ r
    case GoalPropEq(l, r) => l ~=~ r
    case GoalPropReturnsConstr(n, c) =>
      println("Residualizing...")
      val gr = new TheHypergraph with Prettifier {}
      
      val r = new Residualizer(gr, this)
      val resid = r(n.deref).map(_.node)
      
//      val r = new ConstructorsResidualizer(this)
//      val subtree = r(n.deref)
//      val resid = Option(subtree.loadInto2(gr))
      
      println(gr.nodes.size)
      println("Done Residualizing...")
      val res =
        resid match {
          case Some(rt) =>
            val anres = DataflowAnalyzer.returningConstrs(gr)(rt.node) 
            println("Done Analyzing...")
            anres match {
              case None => false
              case Some(x) =>
//                println("\n" + gr.toProg + "\n")
//                println(n.deref.node.prettyDebug)
//                println(x)
                x.subsetOf(Set(c))
            }
          case None => false
        }
      res
    case _ => false
  }
}