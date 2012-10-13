package graphsc

trait Transformations extends Hypergraph {
  
  private def isVar(n: Node): Boolean =
    n.outs.exists(h => h.label.isInstanceOf[Var])
  
  private def isErr(n: Node): Boolean =
    n.outs.exists(h => h.label.isInstanceOf[Error])
  
  private def isInj[T](l: Seq[T]): Boolean = 
    l.distinct == l
  
  private def sequence[T](l: List[List[T]]): List[List[T]] = l match {
    case (h :: t) => for(x <- h; y <- sequence(t)) yield x :: y
    case Nil => List(Nil)
  }
    
  private implicit def injectAt(l: List[RenamedNode]) = new {
    def at(i: Int): RenamedNode =
      if(i < 0 || i >= l.size)
        add(Error(), Nil)
      else
        l(i)
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
    
  // let x = e in x  ->  e
  def letVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Var(), src2, List())) if f1.plain == src2 =>
      val varnum = f1.renaming(0)
      add(Id(), src1, List(es1 at varnum))
  }
  
  def letLet: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Let(), src2, f2 :: es2)) if f1.plain == src2 =>
      val newes = es2.map(e => add(Let(), (f1.renaming comp e) :: es1))
      add(Let(), src1, f2 :: newes)
  }
  
  def letCaseOf: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(Let(), src1, f :: es),
          h2@Hyperedge(CaseOf(cases), src2, g :: hs)) if f.plain == src2 =>
      if(!f.isPlain)
        letCaseOf((
          Hyperedge(Let(), src1, f.plain :: es),
          f.renaming compDests h2))
      else {
        val newg = add(Let(), g :: es)
        val newhs = (cases zip hs).map { case ((_,n),h) =>
            val newes = es.map { e => 
              Renaming((0 until e.arity).map(_ + n).toList) comp e 
            }
            val ys = (0 until n).map(i => variable(i)).toList
            add(Let(), h :: ys ++ newes)
          }
        add(CaseOf(cases), src1, newg :: newhs)
      }
  }
  
  // Construct, Id, Tick, Improvement...
  def letOther: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f :: es),
          Hyperedge(l, src2, gs)) if f.plain == src2 && l.isSimple && gs.nonEmpty =>
      add(l, src1, gs.map(g => add(Let(), (f.renaming comp g) :: es)))
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  // This transformation is performed automatically during normalization, don't use it
  def caseReduce: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Construct(name), src2, args)) if e.plain == src2 =>
      val ((_,n),h) = (cases zip hs).find(_._1._1 == name).get
      assert(n == args.size)
      val bs = 
        args.map(e.renaming comp _) ++ 
        (n until h.arity).map(i => variable(i - n))
      
      add(Let(), src1, List(h) ++ bs)
  }
  
  // propagate positive information
  def caseVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Var(), src2, Nil)) if e.plain == src2 =>
      val varnum = e.renaming(0)
      assert(varnum != -1)
      val newhs =
        (cases zip hs).map { case ((c,n),h) =>
          lazy val value =
            add(Construct(c), (0 until n).map(i => variable(i)).toList)
          val bs = (0 until (h.arity + n)).map { i =>
              if(i == varnum + n)
                value
              else
                variable(i)
            }
          add(Let(), List(h) ++ bs)
        }
      add(CaseOf(cases), src1, e :: newhs)
  }
  
  def caseCase: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(CaseOf(cases2), src2, e2 :: fs2)) if e1.plain == src2 =>
      if(!e1.isPlain)
        caseCase((
          Hyperedge(CaseOf(cases1), src1, e1.plain :: fs1),
          e1.renaming compDests h2))
      else {
        val newfs2 =
          (cases2 zip fs2).map { case ((_,n),f) =>
              val newfs1 = (cases1 zip fs1).map { case ((_,m),g) =>
                  val vec = (0 until g.arity).map(i => 
                    if(i < m) i else i + n).toList
                  Renaming(vec) comp g
                }
              add(CaseOf(cases1), f :: newfs1)
            }
        add(CaseOf(cases2), src1, e2 :: newfs2)
      }
  }
  
  // TODO: There will be a problem if src2 is a dest node of h1 several times
  // Actually this problem holds for all transformations
  // But TransformManager usually replaces the node between the hyperedges with a dummy node...
  def caseTick: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(Tick(), src2, List(e2))) if e1.plain == src2 =>
      val caseof = add(CaseOf(cases1), (e1.renaming comp e2) :: fs1)
      add(Tick(), src1, List(caseof))
    case (h1@Hyperedge(CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(Tick(), src2, List(e2))) if 
            fs1.exists(_.plain == src2) && 
            fs1.forall(f => f.plain == src2 || f.node.outs.exists(_.label.isInstanceOf[Tick])) =>
      val newfs1list =
        sequence(fs1.map(n =>
          if(n.plain == src2) 
            List(h2)
          else
            n.node.outs.toList.filter(_.label.isInstanceOf[Tick])))
      for(l <- newfs1list) {
        val newfs1 = 
          (fs1,l).zipped.map((f,h) => f.renaming comp h.source.renaming.inv comp h.dests(0))
        val caseof = add(CaseOf(cases1), e1 :: newfs1)
        add(Tick(), src1, List(caseof))
      }      
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  // Move the let up, i.e. generalize
  /*def letUp: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(l1, src1, es1),
          h2@Hyperedge(Let(), src2, f2 :: es2)) if 
            (if(l1 == Let()) es1.tail else es1).exists(_.plain == src2) =>
      val (e1, shift) = (es1 zip h1.shifts).find(_._1.plain == src2).get
      // if it is a head of a let then you've found a bug, congratulations
      assert(shift != -1)
      for(e2 <- es2) yield {
        
      }
  }*/
}
