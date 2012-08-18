package graphsc

object Transformations {
  
  def isVar(n: Node): Boolean =
    n.outs.exists(h => h.label.isInstanceOf[Var])
  
  def getVar(n: Node): Int =
    n.outs.collectFirst{ case Hyperedge(Var(a, i), _, _) => i }.get
  
  def isInj[T](l: Seq[T]): Boolean = 
    l.distinct == l
  
  def sequence[T](l: List[List[T]]): List[List[T]] = l match {
    case (h :: t) => for(x <- h; y <- sequence(t)) yield x :: y
    case Nil => List(Nil)
  }
  
  def isRenaming(es: List[Node]): Boolean =
    es.forall(isVar(_)) && isInj(es.map(getVar(_)))
    
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  /*def renamingToId: PartialFunction[Hyperedge, List[Hyperedge]] = {
    case Hyperedge(Renaming(a, vec), src, List(n)) if
      a == vec.length && vec.zipWithIndex.forall{ case (a,b) => a == b } =>
      List(Hyperedge(Id(), src, List(n)))
  }*/
  
  def varToRenaming: PartialFunction[Hyperedge, List[Hyperedge]] = {
    case Hyperedge(Var(a, v), src, Nil) =>
      List(Hyperedge(Renaming(a, List(v)), src, List(Node(Var(1,0), Nil))))
  }
    
  def letToRenaming: PartialFunction[Hyperedge, List[Hyperedge]] = {
    case Hyperedge(Let(a), src, f :: es) if isRenaming(es) =>
      List(Hyperedge(Renaming(a, es.map(getVar(_))), src, List(f)))
  }
  
  def letRenaming: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Renaming(a2, vec), src2, List(f2))) if f1 == src2 =>
      List(Hyperedge(Let(a1), src1, f2 :: vec.map(es1(_))))
  }
  
  def renamingRenaming: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Renaming(a1, vec1), src1, List(f1)),
          Hyperedge(Renaming(a2, vec2), src2, List(f2))) if f1 == src2 =>
      List(Hyperedge(Renaming(a1, vec2.map(vec1(_))), src1, List(f2)))
  }
  
  def rename(ns: List[Node]): List[(List[Int], List[Node])] = {
    // if it is a caseof then ns may have different arities and
    // we mustn't rename variables >= arity
    val arity = if(ns.nonEmpty) ns.map(_.arity).min else -1
    val ll = sequence(ns.map(_.outs.filter(_.label.isInstanceOf[Renaming]).toList))
    for(hs <- ll) yield {
      val pairs = hs.collect { case Hyperedge(r: Renaming, _, List(n)) => (r,n) }
      assert(pairs.size == hs.size)
      val varlist = pairs.map(_._1.vector.take(arity)).flatten.distinct
      val map = varlist.zipWithIndex.toMap
      val newa = varlist.size
      val shift = arity - newa
      
      def varmap(i: Int): Int = 
        if(i < arity) map(i) else i - shift
      
      (varlist, pairs.map { 
          case (r,n) =>
            val k = r.arity - arity
            Node(Renaming(newa + k, r.vector.map(varmap(_))), List(n))
        })
    }
  }
  
  // Move h through multiple renamings
  // now it is actually a canonizer, I should rename it
  def throughRenamings(h: Hyperedge): List[Hyperedge] =
    h.label match {
      case _ if letToRenaming.isDefinedAt(h) =>
        letToRenaming(h)
      case _ if varToRenaming.isDefinedAt(h) =>
        varToRenaming(h)
      case Let(a) =>
        for((v, newdests) <- rename(h.dests.tail)) yield
          Hyperedge(Renaming(a, v), h.source, List(Node(Let(v.size), h.dests(0) :: newdests)))
      case l if l.isSimple && h.dests.nonEmpty =>
        for((v, newdests) <- rename(h.dests)) yield
          Hyperedge(Renaming(h.arity, v), h.source, List(Node(l, newdests)))
      case CaseOf(cases) =>
        for((v, newdests) <- rename(h.dests)) yield
          Hyperedge(Renaming(h.arity, v), h.source, List(Node(CaseOf(cases), newdests)))
      case Renaming(a1, vec1) =>
        val Hyperedge(_, src1, List(f1)) = h
        for(Hyperedge(Renaming(a2, vec2), src2, List(f2)) <- f1.outs.toList) yield
          Hyperedge(Renaming(a1, vec2.map(vec1(_))), src1, List(f2))
      case _ => Nil
    }
  
  def otherRenaming: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(l, src1, List(f1)),
          Hyperedge(r: Renaming, src2, List(f2))) if f1 == src2 && l.isSimple =>
      List(Hyperedge(r, src1, List(Node(l, List(f2)))))
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
    
  def letVar: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Var(a2, i), src2, List())) if f1 == src2 =>
      List(Hyperedge(Id(), src1, List(es1(i))))
  }
  
  def letLet: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Let(a2), src2, f2 :: es2)) if f1 == src2 =>
      val newes = es2.map(e => Node(Let(a1), e :: es1))
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
  
  // Construct, Id, Tick, Improvement...
  def letOther: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(Let(a1), src1, f :: es),
          Hyperedge(l, src2, gs)) if f == src2 && l.isSimple && gs.nonEmpty =>
      List(Hyperedge(l, src1, gs.map(g => Node(Let(a1), g :: es))))
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  def caseReduce: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Construct(name), src2, args)) if e == src2 =>
      val ((_,n),h) = (cases zip hs).find(_._1._1 == name).get
      assert(n == args.size)
      val bs = (0 until e.arity).map(i => Node(Var(e.arity, i), Nil)) ++ args
      // TODO: Tick
      List(Hyperedge(Let(e.arity), src1, List(h) ++ bs))
  }
  
  // propagate positive information
  def caseVar: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Var(ar, v), src2, Nil)) if e == src2 =>
      val newhs =
        (cases zip hs).map { case ((c,n),h) =>
          val value =
            if(n == 0)
              Node(Let(ar + n), List(Node(Construct(c), Nil)))
            else
              Node(Construct(c), (0 until n).map(i => Node(Var(ar + n, ar + i), Nil)).toList)
          val bs = (0 until (ar + n)).map { i =>
              if(i == v)
                value
              else
                Node(Var(ar + n, i), Nil)
            }
          Node(Let(ar + n), List(h) ++ bs)
        }
      List(Hyperedge(CaseOf(cases), src1, e :: newhs))
  }
  
  def caseCase: PartialFunction[(Hyperedge, Hyperedge), List[Hyperedge]] = {
    case (Hyperedge(CaseOf(cases1), src1, e1 :: hs1),
          Hyperedge(CaseOf(cases2), src2, e2 :: hs2)) if e1 == src2 =>
      val newhs2 =
        (cases2 zip hs2).map { case ((_,n),h) =>
            val newhs1 = (cases1 zip hs1).map { case ((_,m),g) =>
                val bs = (0 until g.arity).map(i => 
                  if(i < g.arity - m)
                    Node(Var(h.arity + m, i), Nil)
                  else
                    Node(Var(h.arity + m, i + n), Nil))
                Node(Let(h.arity + m), List(g) ++ bs)
              }
            Node(CaseOf(cases1), h :: newhs1)
          }
      List(Hyperedge(CaseOf(cases2), src1, e2 :: newhs2))
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

}
