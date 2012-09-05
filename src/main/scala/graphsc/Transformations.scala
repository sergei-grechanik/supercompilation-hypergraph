package graphsc

trait Transformations extends Hypergraph {
  
  private def isVar(n: Node): Boolean =
    n.outs.exists(h => h.label.isInstanceOf[Var])
  
  private def getVar(n: Node): Int =
    n.outs.collectFirst{ case Hyperedge(Var(i), _, _) => i }.get
  
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
    case Hyperedge(Renaming(vec), src, List(n)) if
      vec.zipWithIndex.forall{ case (a,b) => a == b } =>
      add(Id(), src, List(n))
  }
  
  // this transformation must be done automatically by TheHypergraph
  def renamingInv: PartialFunction[Hyperedge, Unit] = {
    case Hyperedge(Renaming(vec), src, List(n)) if
      vec.toSet == (0 until vec.size).toSet =>
      val newvec = vec.zipWithIndex.sortBy(_._1).map(_._2)
      add(Renaming(newvec), n, List(src))
  }
    
  def renamingVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Renaming(vec1), src1, List(f1)),
          Hyperedge(Var(i), src2, Nil)) if f1 == src2 =>
      if(vec1(i) >= 0)
        add(Var(vec1(i)), src1, Nil)
      else
        add(Error(), src1, Nil)
  }
  
  def varToRenaming: PartialFunction[Hyperedge, Unit] = {
    case Hyperedge(Var(v), src, Nil) =>
      add(Renaming(List(v)), src, List(add(Var(0), Nil)))
  }
    
  // TODO: Should be binary
  def letToRenaming: PartialFunction[Hyperedge, Unit] = {
    case Hyperedge(Let(), src, f :: es) if isRenaming(es) =>
      add(Renaming(es.map(getVar(_))), src, List(f))
  }
  
  def letRenaming: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Renaming(vec), src2, List(f2))) if f1 == src2 =>
      add(Let(), src1, f2 :: vec.map(i => 
        if(i < 0 || i >= es1.size) add(Error(), Nil) else es1(i)))
  }
  
  def renamingRenaming: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(r: Renaming, src1, List(f1)),
          Hyperedge(Renaming(vec2), src2, List(f2))) if f1 == src2 =>
      add(Renaming(vec2.map(r(_))), src1, List(f2))
  }
  
  def rename(ns: List[Node], ren: Hyperedge = null, shifts1: List[Int] = null): 
      List[(List[Int], List[Node])] = {
    val shifts = if(shifts1 == null) ns.map(_ => 0) else shifts1
    
    val ll = sequence(ns.map(n => 
      if(ren != null && ren.source == n)
        List(ren)
      else
        n.outs.collect{
          case o if o.label.isInstanceOf[Renaming] => o
        } toList))
        /*n.outs.collect {
          case o if o.label.isInstanceOf[Renaming] => o
          case o => Hyperedge(Renaming(0 until n.arity toList), null, List(n))
        }.toList))*/
        
    println("Renamings give " + ll.size)
    if(ll.size == 0)
      println("")
    
    for(hs <- ll) yield {
      val pairs = hs.collect { case Hyperedge(r: Renaming, _, List(n)) => (r,n) }
      assert(pairs.size == hs.size)
      val varlist = (pairs zip shifts).map {
        case (p,s) => p._1.vector.map(_ - s).filter(_ >= 0)}.flatten.distinct
      val map = varlist.zipWithIndex.toMap
      
      def varmap(i: Int): Int = 
        if(i < 0) i else map(i)
      
      (varlist, (pairs zip shifts).map { 
          case ((r,n),s) =>
            add(Renaming(r.vector.map(i => varmap(i - s) + s)), List(n))
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
      case Let() =>
        val lst =
        for((v, newdests) <- rename(h.dests.tail)) yield
          add(Renaming(v), h.source, List(add(Let(), h.dests(0) :: newdests)))
        //if(lst.isEmpty) addHyperedge(h)
      case l if l.isSimple && h.dests.nonEmpty =>
        val lst =
        for((v, newdests) <- rename(h.dests)) yield
          add(Renaming(v), h.source, List(add(l, newdests)))
        //if(lst.isEmpty) addHyperedge(h)
      case CaseOf(cases) =>
        val lst =
        for((v, newdests) <- rename(h.dests, null, 0 :: cases.map(_._2))) yield
          add(Renaming(v), h.source, List(add(CaseOf(cases), newdests)))
        //if(lst.isEmpty) addHyperedge(h)
      case r: Renaming =>
        val Hyperedge(_, src1, List(f1)) = h
        val lst =
        for(Hyperedge(Renaming(vec2), src2, List(f2)) <- f1.outs.toList) yield
          add(Renaming(vec2.map(r(_))), src1, List(f2))
        if(lst.isEmpty) addHyperedge(h)
      case Construct(_) =>
        addHyperedge(h)
        add(Renaming(Nil), h.source, List(h.source))
      case Error() =>
        addHyperedge(h)
        add(Renaming(Nil), h.source, List(h.source))
      case _ =>
        throw new Exception("This case should be impossible, I forgot something here")
    }
  
  def anyRenaming: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(l1, src1, ds1), 
          h2@Hyperedge(Renaming(vec2), src2, List(f2))) if ds1.contains(src2) =>
      l1 match {
        case Let() if ds1(0) == src2 =>
          letRenaming(h1, h2)
        case Let() =>
          for((v, newdests) <- rename(ds1.tail, h2)) yield
            Hyperedge(Renaming(v), src1, List(add(Let(), ds1(0) :: newdests)))
        case l if l.isSimple =>
          for((v, newdests) <- rename(ds1, h2)) yield
            Hyperedge(Renaming(v), src1, List(add(l, newdests)))
        case CaseOf(cases) =>
          for((v, newdests) <- rename(ds1, h2, 0 :: cases.map(_._2))) yield
            Hyperedge(Renaming(v), src1, List(add(CaseOf(cases), newdests))) 
        case r: Renaming =>
          add(Renaming(vec2.map(r(_))), src1, List(f2))
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
    case (Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Var(i), src2, List())) if f1 == src2 =>
      add(Id(), src1, List(es1(i)))
    case (h1@Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Var(i), src2, List())) if es1.contains(src2) && 
                                              letToRenaming.isDefinedAt(h1) =>
      letToRenaming(h1)
  }
  
  def letLet: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Let(), src2, f2 :: es2)) if f1 == src2 =>
      val newes = es2.map(e => add(Let(), e :: es1))
      add(Let(), src1, f2 :: newes)
  }
  
  def letCaseOf: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f :: es),
          Hyperedge(CaseOf(cases), src2, g :: hs)) if f == src2 =>
      val newg = add(Let(), g :: es)
      val newhs = (cases zip hs).map { case ((_,n),h) =>
          val newes = es.map { e => 
            add(Renaming((0 until e.arity).map(_ + n).toList), List(e)) 
          }
          val ys = (0 until n).map(i => add(Var(i), Nil)).toList
          add(Let(), h :: ys ++ newes)
        }
      add(CaseOf(cases), src1, newg :: newhs)
  }
  
  // Construct, Id, Tick, Improvement...
  def letOther: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f :: es),
          Hyperedge(l, src2, gs)) if f == src2 && l.isSimple && gs.nonEmpty =>
      add(l, src1, gs.map(g => add(Let(), g :: es)))
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  def caseReduce: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Construct(name), src2, args)) if e == src2 =>
      val ((_,n),h) = (cases zip hs).find(_._1._1 == name).get
      assert(n == args.size)
      val bs = args ++ (n until h.arity).map(i => add(Var(i - n), Nil))
      // TODO: Tick
      add(Let(), src1, List(h) ++ bs)
  }
  
  // propagate positive information
  def caseVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Var(v), src2, Nil)) if e == src2 =>
      val newhs =
        (cases zip hs).map { case ((c,n),h) =>
          val value =
            add(Construct(c), (0 until n).map(i => add(Var(i), Nil)).toList)
          val bs = (0 until (h.arity + n)).map { i =>
              if(i == v + n)
                value
              else
                add(Var(i), Nil)
            }
          add(Let(), List(h) ++ bs)
        }
      add(CaseOf(cases), src1, e :: newhs)
  }
  
  def caseCase: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases1), src1, e1 :: hs1),
          Hyperedge(CaseOf(cases2), src2, e2 :: hs2)) if e1 == src2 =>
      val newhs2 =
        (cases2 zip hs2).map { case ((_,n),h) =>
            val newhs1 = (cases1 zip hs1).map { case ((_,m),g) =>
                val vec = (0 until g.arity).map(i => 
                  if(i <= m) i else i + n).toList
                add(Renaming(vec), List(g))
              }
            add(CaseOf(cases1), h :: newhs1)
          }
      add(CaseOf(cases2), src1, e2 :: newhs2)
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

}
