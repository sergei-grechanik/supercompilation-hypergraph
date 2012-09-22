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
  // let x = x, y = y in e  ->  theta e
  def letVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Var(), src2, List())) if f1.plain == src2 =>
      val varnum = f1.renaming(0)
      add(Id(), src1, List(es1 at varnum))
    // TODO: I don't know if we really need this transformation
    // but at least it should make sure that there is no variable gluing
    /*case (h1@Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Var(), src2, List())) if es1.forall(_.plain == src2) =>
      val ren = Renaming(es1.map(_.renaming(0)))
      add(Id(), src1, List(ren comp f1))*/
  }
  
  def letLet: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(Let(), src1, f1 :: es1),
          Hyperedge(Let(), src2, f2 :: es2)) if f1 == src2 =>
      val newes = es2.map(e => add(Let(), e :: es1))
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
  
  def caseReduce: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (Hyperedge(CaseOf(cases), src1, e :: hs),
          Hyperedge(Construct(name), src2, args)) if e.plain == src2 =>
      val ((_,n),h) = (cases zip hs).find(_._1._1 == name).get
      assert(n == args.size)
      val bs = 
        args.map(e.renaming comp _) ++ 
        (n until h.arity).map(i => variable(i - n))
      // TODO: Tick
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
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

}
