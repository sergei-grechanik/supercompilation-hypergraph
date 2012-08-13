package graphsc

object Transformations {
  
  def letVar: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Var(a2, i), src2, List())) if f1 == src2 =>
      List(Hyperedge(Id(), src1, List(es1(i))))
  }
  
  def letLet: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Let(a2), src2, f2 :: es2)) if f1 == src2 =>
      val newes = es2.map(e => Node(Let(a2), e :: es1))
      List(Hyperedge(Let(a1), src1, f2 :: newes))
  }
  
  def letCaseOf: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(a1), src1, f :: es),
          Hyperedge(CaseOf(cases), src2, g :: hs)) if f == src2 =>
      val newg = Node(Let(a1), g :: es)
      val newhs = (cases zip hs).map { case ((_,n),h) =>
          val newes = es.map { e => 
            Node(Let(a1 + n), e :: 
                (0 until a1).map(i => Node(Var(a1 + n, i), Nil)).toList) 
          }
          val ys = (0 until n).map(i => Node(Var(a1 + n, a1 + i), Nil))
          Node(Let(a1 + n), h :: newes ++ ys)
        }
      List(Hyperedge(CaseOf(cases), src1, newg :: newhs))
  }
  
  /*
  private def isOrdered[T](l: List[T])(implicit ord: Ordering[T]): Boolean =
    (l, l.tail).zipped.forall(ord.lteq(_, _))
    
  // Remove unused bound variables and sort them
  def letSimplify: PartialFunction[Hyperedge, List[Hyperedge]] = {
    case Hyperedge(Let(xs), src, f :: es) if xs.exists(!f.used(_)) || !isOrdered(xs) =>
      import Renaming._
      println("f.used = " + f.used)
      val oldxsset = xs.toSet
      println("oldxsset = " + oldxsset)
      val newxes = (xs zip es).filter(f used _._1).sortBy(_._1)
      val newxs = newxes.map(_._1)
      println("newxs = " + newxs)
      val newes = newxes.map(_._2)
      val newxsset = newxs.toSet
      val n = f.varCount
      println("n = " + n)
      val sirnew = shiftInvRenaming(newxsset, n)
      println("sirnew = " + sirnew)
      val sirold = shiftInvRenaming(oldxsset, n)
      println("sirold = " + sirold)
      println("sirold.inv = " + sirold.inv)
      println("sirnew . sirold.inv = " + (sirnew comp sirold.inv))
      val theta = 
        (sirnew comp sirold.inv) reduce f.used
      println("theta = " + theta)
      if(theta.isId)
        List(Hyperedge(Let(newxs), src, f :: newes))
      else {
        val f1 = Hyperedge(theta, List(f))
        List(Hyperedge(Let(newxs), src, f1.source :: newes), f1)
      }
  }
  
  // let x = e in x  ->  e
  def letVar: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(xs), src, f :: es), Hyperedge(Var(), f1, List())) if f == f1 =>
      List(Hyperedge(Renaming(), src, List(es(0))))
  }
  
  // let x = e in θ f  ->  θ' let θx = θ'^-1 e in f
  def letRenaming: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(xs), src, f :: es), 
          Hyperedge(ren: Renaming, f1, List(dst))) if f == f1 =>
      val (theta, newxs) = ren.upThroughBinding(xs)
      val thetainv = theta.inv
      val newxes = (newxs zip es).sortBy(_._1)
      val newes = newxes.map(e => Hyperedge(thetainv, List(e._2)))
      val dst1 = Hyperedge(Let(newxes.map(_._1)), dst :: newes.map(_.source))
      List(Hyperedge(theta, src, List(dst1.source)), dst1) ++ newes
  }
  
  // let x = e in case g of h  ->  case (let x = e in g) of (let shInv x = shInv e in h)
  def letCaseOf: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(xs), src, f :: es), 
          Hyperedge(CaseOf(cases), f1, g :: hs)) if f == f1 =>
      val newg = Node(Let(xs), g :: es)
      val varcount = (xs.max + 1) max es.map(_.varCount).max
      val newhs = (cases zip hs).map { case ((_,bnd), h) =>
        val shinv = Renaming.shiftInvRenaming(bnd.toSet, varcount)
        val shinves = es.map(e => Node(shinv, List(e)))
        Node(Let(xs.map(shinv(_))), h :: shinves)
      }
      List(Hyperedge(CaseOf(cases), src, newg :: newhs))
  }
    
  
  // let e in (a + b) -> (let e in a) + (let e in b)
  // let x = e in x -> e
  def letDown(g: Hypergraph, let: Hyperedge) = let match {
    case Hyperedge(Let(xs), src, f :: es) =>
      val usedxes = (xs zip es).filter(f used _._1)
      if(usedxes.isEmpty) {
        g.transforming(let)
        g.glueNodes(f, src)
      } else {
        for(h <- f.outs) h.label match {
          case Var() =>
            g.transforming(let, h)
            g.glueNodes(es(0), src)
          case ren : Renaming =>
            g.transforming(let, h)
            val ner = ren.inv
            val usedxes1 = usedxes.map{
                case (x,e) =>
                  val newe = g.add(ner, List(e))
                  (ner(x), newe)
              }.sortBy(_._1)
            val newlet = 
              g.add(Let(usedxes1.map(_._1)), h.dests(0) :: usedxes1.map(_._2))
            g.add(ren, src, List(newlet))
          case lab =>
            g.transforming(let, h)
            val dests =
              for((d, i) <- h.dests.zipWithIndex) yield {
                val usedxes1 = usedxes.filter {
                    case (x,e) => !lab.bound(i).contains(x) && d.used(x)
                  }
                if(usedxes1.isEmpty)
                  d
                else
                  g.add(Let(usedxes1.map(_._1)), d :: usedxes1.map(_._2))
              }
            g.add(Hyperedge(lab, src, dests))
        }
      }
    case _ =>
  }
  
  // propagate positive information
  def propagate(g: Hypergraph, cas: Hyperedge) = cas match {
    case Hyperedge(CaseOf(cases), src, x :: dests)
      if x.outs.exists(_.label == Var) =>
        g.transforming(cas)
        // Var returns the zeroth variable
        val v = x //g.add(Var(), List())
        val newdests = 
          for(((d, i_1), (name,varnums)) <- dests.zipWithIndex zip cases) yield 
            if(CaseOf(cases).bound(i_1 + 1).contains(0) || !d.used(0)) {
              d
            } else {              
              val vars = 
                v :: varnums.map { j => 
                  g.add(Renaming(0 -> j), List(v))
                }
                  
              val newe = g.add(Construct(name), vars)
              g.add(Let(List(0)), List(d, newe))
            }
        
        g.add(Hyperedge(CaseOf(cases), src, x :: newdests))
    case _ =>
  }
  
  // case (S e) of { S x -> f(x) } -> let x = e in f(x)
  def caseReduce(g: Hypergraph, cas: Hyperedge) = cas match {
    case Hyperedge(CaseOf(cases), src, x :: dests) =>
      // we assume that there is only one outgoing construct hyperedge
      // if it is not true then it is a bug
      x.outs.find(_.label.isInstanceOf[Construct]) match {
        case Some(h@Hyperedge(Construct(name), _, exprs)) =>
          (cases zip dests).find(_._1._1 == name) match {
            case Some(((_,vars),d)) =>
              g.transforming(cas, h)
              val xes = (vars zip exprs).sortBy(_._1)
              val let = g.add(Let(xes.map(_._1)), d :: xes.map(_._2))
              g.glueNodes(src, let)
            case _ =>
          }
        case _ =>
      }
    case _ =>
  }
  
  // case (case e of ... -> f) of ... -> case e of ... -> case f of ...
  def caseCase(g: Hypergraph, cas: Hyperedge) = cas match {
    case Hyperedge(CaseOf(cases1), src, e1 :: dests1) =>
      for(h@Hyperedge(CaseOf(cases2), _, e2 :: dests2) <- e1.outs) {
        g.transforming(cas, h)
        val newdests = dests2.map{d => 
          g.add(CaseOf(cases1), d :: dests1)
        }
        g.add(Hyperedge(CaseOf(cases2), src, e2 :: newdests))
      }
    case _ =>
  }
  
  def throughRenaming(g: Hypergraph, h: Hyperedge) = h match {
    case Hyperedge(r@Renaming(_), src, List(d)) if r.isId =>
      g.transforming(h)
      g.glueNodes(src, d)
    case Hyperedge(r1@Renaming(_), src, List(d1)) =>
      for(h1@Hyperedge(r2@Renaming(_), _, List(d2)) <- d1.outs) {
        g.transforming(h, h1)
        val rcomp = r1 comp r2
        if(rcomp.isId)
          g.glueNodes(src, d2)
        else
          g.add(Hyperedge(rcomp, src, List(d2)))
      }
    case Hyperedge(Let(xs), src, f :: es) =>
      val usedxes = (xs zip es).filter(f used _._1)
      if(usedxes.nonEmpty)
        for(h1@Hyperedge(r@Renaming(_), _, List(d2)) <- f.outs) {
          g.transforming(h, h1)
          val ner = r.inv
          val usedxes1 = usedxes.map{
              case (x,e) =>
                val newe = g.add(ner, List(e))
                (ner(x), newe)
            }.sortBy(_._1)
          val newlet = 
            g.add(Let(usedxes1.map(_._1)), h.dests(0) :: usedxes1.map(_._2))
          g.add(Hyperedge(r, src, List(newlet)))
        }
    case Hyperedge(CaseOf(cases), src, e :: dests) =>
      for(h1@Hyperedge(r@Renaming(_), _, List(e2)) <- e.outs) {
        g.transforming(h, h1)
        val rinv = r.inv
        val newvarsdests =
          for(((name, vars), d) <- cases zip dests) yield {
            ((name, vars.map(rinv(_))), 
                g.add(Hyperedge(rinv, null, List(d))).source)
          }
        val newcases = newvarsdests.map(_._1)
        val newdests = newvarsdests.map(_._2)
        val newcase = g.add(Hyperedge(CaseOf(newcases), null, e2 :: newdests)).source
        g.add(Hyperedge(r, src, List(newcase)))
      }
    case Hyperedge(l, src, e :: dests) =>
      for(h1@Hyperedge(r@Renaming(_), _, List(e2)) <- e.outs) {
        g.transforming(h, h1)
        val rinv = r.inv
        val newdests =
          for(d <- dests) yield
            g.add(Hyperedge(rinv, null, List(d))).source
        val newh = g.add(Hyperedge(l, null, e2 :: newdests)).source
        g.add(Hyperedge(r, src, List(newh)))
      }
    case _ =>
  }
  
  def glueAll(g: TheHypergraph) {
    var changed = false
    for(n <- g.nodes)
      for(h <- n.outs) h match {
        case Hyperedge(Let(xs), src, f :: es) if !xs.exists(f used _) && f != src =>
          g.transforming(h)
          g.glueNodes(f, src)
          changed = true
        case Hyperedge(r@Renaming(_), src, List(d)) if r.isId && src != d =>
          g.transforming(h)
          g.glueNodes(src, d)
          changed = true
        case _ =>
      }
    if(changed)
      glueAll(g)
  }
  
  // This transformation is done automatically by TheHyperedge when adding a renaming
  def reverseRenaming(g: TheHypergraph, h: Hyperedge) = h match {
    case Hyperedge(r@Renaming(_), src, List(d)) if !r.isId && src != d =>
      g.transforming(h)
      g.add(r.inv, d, List(src))
    case _ =>
  }*/
}
