package graphsc

object Transformations {
  // let e in (a + b) -> (let e in a) + (let e in b)
  // let x = e in x -> e
  def letDown(g: Hypergraph, let: Hyperedge) = let match {
    case Hyperedge(Let(x), src, List(f, e)) if x < f.arity =>
      for(h <- f.outs) h.label match {
        case ren : Renaming =>
          val ner = ren.inv
          if(x >= ner.arity)
            g.addHyperedge(h.from(src))
          else {
            val newe = g.addHyperedge(Hyperedge(ner, null, List(e))).source
            val newlet = g.addHyperedge(Hyperedge(Let(ner(x)), null, List(h.dests(0), newe))).source
            g.addHyperedge(Hyperedge(ren, src, List(newlet)))
          }
        case lab =>
          val dests =
            for((d, i) <- h.dests.zipWithIndex) yield {
              if(lab.bound(i).contains(x) || x >= d.arity)
                d
              else
                g.addHyperedge(Hyperedge(Let(x), null, List(d, e))).source
            }
          g.addHyperedge(Hyperedge(lab, src, dests))
      }
    case Hyperedge(Let(x), src, List(f, e)) if x >= f.arity =>
      g.glueNodes(f, src)
    case _ =>
  }
  
  // propagate positive information
  def propagate(g: Hypergraph, cas: Hyperedge) = cas match {
    case Hyperedge(CaseOf(cases), src, x :: dests)
      if x.outs.exists(_.label == Var) =>
        // Var returns the zeroth variable
        val v = g.addHyperedge(Hyperedge(Var(), null, List())).source
        val newdests = 
          for(((d, i_1), (name,varnums)) <- dests.zipWithIndex zip cases) yield 
            if(CaseOf(cases).bound(i_1 + 1).contains(0)) {
              d
            } else {              
              val vars = 
                v :: varnums.map { j => 
                  g.addHyperedge(
                      Hyperedge(new Renaming(0 -> j), null, List(v))
                    ).source
                }
                  
              val newe = g.addHyperedge(Hyperedge(Construct(name), null, vars)).source
              g.addHyperedge(Hyperedge(Let(0), null, List(d, newe))).source
            }
        
        g.addHyperedge(Hyperedge(CaseOf(cases), src, x :: newdests))
    case _ =>
  }
  
  // case (S e) of { S x -> f(x) } -> let x = e in f(x)
  def caseReduce(g: Hypergraph, cas: Hyperedge) = cas match {
    case Hyperedge(CaseOf(cases), src, x :: dests) =>
      // we assume that there is only one outgoing construct hyperedge
      // might not be true in general
      x.outs.find(_.label.isInstanceOf[Construct]) match {
        case Some(Hyperedge(Construct(name), _, exprs)) =>
          (cases zip dests).find(_._1._1 == name) match {
            case Some(((_,vars),d)) =>
              var res = d
              for((v,e) <- vars zip exprs) {
                res = g.addHyperedge(Hyperedge(Let(v), null, List(res, e))).source
              }
              g.glueNodes(src, res)
            case _ =>
          }
        case _ =>
      }
    case _ =>
  }
  
  // case (case e of ... -> f) of ... -> case e of ... -> case f of ...
  def caseCase(g: Hypergraph, cas: Hyperedge) = cas match {
    case Hyperedge(CaseOf(cases1), src, e1 :: dests1) =>
      for(Hyperedge(CaseOf(cases2), _, e2 :: dests2) <- e1.outs) {
        val newdests = dests2.map{d => 
          g.addHyperedge(Hyperedge(CaseOf(cases1), null, d :: dests1)).source
        }
        g.addHyperedge(Hyperedge(CaseOf(cases2), src, e2 :: newdests))
      }
    case _ =>
  }
  
  def throughRenaming(g: Hypergraph, h: Hyperedge) = h match {
    case Hyperedge(r@Renaming(_), src, List(d)) if r.isId =>
      g.glueNodes(src, d)
    case Hyperedge(r1@Renaming(_), src, List(d1)) =>
      for(Hyperedge(r2@Renaming(_), _, List(d2)) <- d1.outs) {
        g.addHyperedge(Hyperedge(r1 comp r2, src, List(d2)))
      }
    case Hyperedge(Let(x), src, List(d, e)) if x < d.arity =>
      for(Hyperedge(r@Renaming(_), _, List(d2)) <- d.outs) {
        val rinv = r.inv
        val y = rinv(x)
        val newe = g.addHyperedge(Hyperedge(rinv, null, List(e))).source
        val newlet = g.addHyperedge(Hyperedge(Let(y), null, List(d2, newe))).source
        g.addHyperedge(Hyperedge(r, src, List(newlet)))
      }
    case Hyperedge(CaseOf(cases), src, e :: dests) =>
      for(Hyperedge(r@Renaming(_), _, List(e2)) <- e.outs) {
        val rinv = r.inv
        val newvarsdests =
          for(((name, vars), d) <- cases zip dests) yield {
            ((name, vars.map(rinv(_))), 
                g.addHyperedge(Hyperedge(rinv.widen(d.arity), null, List(d))).source)
          }
        val newcases = newvarsdests.map(_._1)
        val newdests = newvarsdests.map(_._2)
        val newcase = g.addHyperedge(Hyperedge(CaseOf(newcases), null, e2 :: newdests)).source
        g.addHyperedge(Hyperedge(r.widen(newcase.arity), src, List(newcase)))
      }
    case Hyperedge(l, src, e :: dests) =>
      for(Hyperedge(r@Renaming(_), _, List(e2)) <- e.outs) {
        val rinv = r.inv
        val newdests =
          for(d <- dests) yield
            g.addHyperedge(Hyperedge(rinv, null, List(d))).source
        val newh = g.addHyperedge(Hyperedge(l, null, e2 :: newdests)).source
        g.addHyperedge(Hyperedge(r, src, List(newh)))
      }
    case _ =>
  }
}
