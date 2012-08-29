package graphsc

trait Transformations extends Hypergraph {
  
  private def isVar(n: Node): Boolean =
    n.outs.exists(h => h.label.isInstanceOf[Var])
  
  private def getVar(n: Node): Int =
    n.outs.collectFirst{ case Hyperedge(Var(a, i), _, _) => i }.get
  
  private def isInj[T](l: Seq[T]): Boolean = 
    l.distinct == l
  
  private def sequence[T](l: List[List[T]]): List[List[T]] = l match {
    case (h :: t) => for(x <- h; y <- sequence(t)) yield x :: y
    case Nil => List(Nil)
  }
  
  def isRenaming(es: List[Node]): Boolean =
    es.forall(isVar(_)) && isInj(es.map(getVar(_)))
    
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  // this transformation must be done automatically by TheHypergraph
  def renamingToId: PartialFunction[Hyperedge, Unit] = {
    case Hyperedge(Renaming(a, vec), src, List(n)) if
      a == vec.length && vec.zipWithIndex.forall{ case (a,b) => a == b } =>
      add(Id(), src, List(n))
  }
    
  def renamingVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Renaming(a1, vec1), src1, List(f1)),
          Hyperedge(Var(a2, i), src2, Nil)) if f1 == src2 =>
      add(Var(a1, vec1(i)), src1, Nil)
  }
  
  def varToRenaming: PartialFunction[Hyperedge, Unit] = {
    case Hyperedge(Var(a, v), src, Nil) =>
      add(Renaming(a, List(v)), src, List(add(Var(1,0), Nil)))
  }
    
  def letToRenaming: PartialFunction[Hyperedge, Unit] = {
    case Hyperedge(Let(a), src, f :: es) if isRenaming(es) =>
      add(Renaming(a, es.map(getVar(_))), src, List(f))
  }
  
  def letRenaming: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Renaming(a2, vec), src2, List(f2))) if f1 == src2 =>
      add(Let(a1), src1, f2 :: vec.map(es1(_)))
  }
  
  def renamingRenaming: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Renaming(a1, vec1), src1, List(f1)),
          Hyperedge(Renaming(a2, vec2), src2, List(f2))) if f1 == src2 =>
      add(Renaming(a1, vec2.map(vec1(_))), src1, List(f2))
  }
  
  def rename(ns: List[Node], ren: Hyperedge = null): List[(List[Int], List[Node])] = {
    // if it is a caseof then ns may have different arities and
    // we mustn't rename variables >= arity
    val arity = if(ns.nonEmpty) ns.map(_.arity).min else -1
    
    val ll = sequence(ns.map(n => 
      if(ren != null && ren.source == n)
        List(ren)
      else
        n.outs.filter(_.label.isInstanceOf[Renaming]).toList))
        
    println("Renamings give " + ll.size)
    
    for(hs <- ll) yield {
      val pairs = hs.collect { case Hyperedge(r: Renaming, _, List(n)) => (r,n) }
      assert(pairs.size == hs.size)
      val varlist = pairs.map(_._1.vector.filter(_ < arity)).flatten.distinct
      val map = varlist.zipWithIndex.toMap
      val newa = varlist.size
      val shift = arity - newa
      
      def varmap(i: Int): Int = 
        if(i < arity) map(i) else i - shift
      
      (varlist, pairs.map { 
          case (r,n) =>
            val k = r.arity - arity
            add(Renaming(newa + k, r.vector.map(varmap(_))), List(n))
        })
    }
  }
  
  // Move h through multiple renamings
  // now it is actually a canonizer, I should rename it
  def throughRenamings(h: Hyperedge): Unit =
    h.label match {
      case _ if letToRenaming.isDefinedAt(h) =>
        letToRenaming(h)
      case _ if varToRenaming.isDefinedAt(h) =>
        varToRenaming(h)
      case Let(a) =>
        val lst =
        for((v, newdests) <- rename(h.dests.tail)) yield
          add(Renaming(a, v), h.source, List(add(Let(v.size), h.dests(0) :: newdests)))
        if(lst.isEmpty) addHyperedge(h)
      case l if l.isSimple && h.dests.nonEmpty =>
        val lst =
        for((v, newdests) <- rename(h.dests)) yield
          add(Renaming(h.arity, v), h.source, List(add(l, newdests)))
        if(lst.isEmpty) addHyperedge(h)
      case CaseOf(cases) =>
        val lst =
        for((v, newdests) <- rename(h.dests)) yield
          add(Renaming(h.arity, v), h.source, List(add(CaseOf(cases), newdests)))
        if(lst.isEmpty) addHyperedge(h)
      case Renaming(a1, vec1) =>
        val Hyperedge(_, src1, List(f1)) = h
        val lst =
        for(Hyperedge(Renaming(a2, vec2), src2, List(f2)) <- f1.outs.toList) yield
          add(Renaming(a1, vec2.map(vec1(_))), src1, List(f2))
        if(lst.isEmpty) addHyperedge(h)
      case _ =>
    }
  
  def anyRenaming: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(l1, src1, ds1), 
          h2@Hyperedge(Renaming(a2, vec2), src2, List(f2))) if ds1.contains(src2) =>
      l1 match {
        case Let(a1) if ds1(0) == src2 =>
          letRenaming(h1, h2)
        case Let(a1) =>
          for((v, newdests) <- rename(ds1.tail, h2)) yield
            Hyperedge(Renaming(a1, v), src1, List(add(Let(v.size), ds1(0) :: newdests)))
        case l if l.isSimple =>
          for((v, newdests) <- rename(ds1, h2)) yield
            Hyperedge(Renaming(h1.arity, v), src1, List(add(l, newdests)))
        case CaseOf(cases) =>
          for((v, newdests) <- rename(ds1, h2)) yield
            Hyperedge(Renaming(h1.arity, v), src1, List(add(CaseOf(cases), newdests))) 
        case Renaming(a1, vec1) =>
          add(Renaming(a1, vec2.map(vec1(_))), src1, List(f2))
        case _ =>
          // Impossible
          throw new Exception("There is a bug")
      }
  }
  
  def otherRenaming: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(l, src1, List(f1)),
          Hyperedge(r: Renaming, src2, List(f2))) if f1 == src2 && l.isSimple =>
      add(r, src1, List(add(l, List(f2))))
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
    
  // let x = e in x  ->  e
  // let x = x, y = y in e  ->  theta e
  def letVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Var(a2, i), src2, List())) if f1 == src2 =>
      add(Id(), src1, List(es1(i)))
    case (h1@Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Var(a2, i), src2, List())) if es1.contains(src2) && 
                                                  letToRenaming.isDefinedAt(h1) =>
      letToRenaming(h1)
  }
  
  def letLet: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(a1), src1, f1 :: es1),
          Hyperedge(Let(a2), src2, f2 :: es2)) if f1 == src2 =>
      val newes = es2.map(e => add(Let(a1), e :: es1))
      add(Let(a1), src1, f2 :: newes)
  }
  
  def letCaseOf: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(a1), src1, f :: es),
          Hyperedge(CaseOf(cases), src2, g :: hs)) if f == src2 =>
      val newg = add(Let(a1), g :: es)
      val newhs = (cases zip hs).map { case ((_,n),h) =>
          val newes = es.map { e => 
            add(Let(a1 + n), e :: 
                (0 until a1).map(i => add(Var(a1 + n, i), Nil)).toList) 
          }
          val ys = (0 until n).map(i => add(Var(a1 + n, a1 + i), Nil))
          add(Let(a1 + n), h :: newes ++ ys)
        }
      add(CaseOf(cases), src1, newg :: newhs)
  }
  
  // Construct, Id, Tick, Improvement...
  def letOther: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(a1), src1, f :: es),
          Hyperedge(l, src2, gs)) if f == src2 && l.isSimple && gs.nonEmpty =>
      add(l, src1, gs.map(g => add(Let(a1), g :: es)))
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  def caseReduce: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Construct(name), src2, args)) if e == src2 =>
      val ((_,n),h) = (cases zip hs).find(_._1._1 == name).get
      assert(n == args.size)
      val bs = (0 until e.arity).map(i => add(Var(e.arity, i), Nil)) ++ args
      // TODO: Tick
      add(Let(e.arity), src1, List(h) ++ bs)
  }
  
  // propagate positive information
  def caseVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Var(ar, v), src2, Nil)) if e == src2 =>
      val newhs =
        (cases zip hs).map { case ((c,n),h) =>
          val value =
            if(n == 0)
              add(Let(ar + n), List(add(Construct(c), Nil)))
            else
              add(Construct(c), (0 until n).map(i => add(Var(ar + n, ar + i), Nil)).toList)
          val bs = (0 until (ar + n)).map { i =>
              if(i == v)
                value
              else
                add(Var(ar + n, i), Nil)
            }
          add(Let(ar + n), List(h) ++ bs)
        }
      add(CaseOf(cases), src1, e :: newhs)
  }
  
  def caseCase: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases1), src1, e1 :: hs1),
          Hyperedge(CaseOf(cases2), src2, e2 :: hs2)) if e1 == src2 =>
      val newhs2 =
        (cases2 zip hs2).map { case ((_,n),h) =>
            val newhs1 = (cases1 zip hs1).map { case ((_,m),g) =>
                val bs = (0 until g.arity).map(i => 
                  if(i < g.arity - m)
                    add(Var(h.arity + m, i), Nil)
                  else
                    add(Var(h.arity + m, i + n), Nil))
                add(Let(h.arity + m), List(g) ++ bs)
              }
            add(CaseOf(cases1), h :: newhs1)
          }
      add(CaseOf(cases2), src1, e2 :: newhs2)
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

}
