package graphsc
package transformation

import scala.annotation.tailrec

trait Transformations extends Hypergraph {
  
  private def isVar(n: Node): Boolean =
    n.outs.exists(h => h.label.isInstanceOf[Var])
  
  private def isUnused(n: Node): Boolean =
    n.outs.exists(h => h.label.isInstanceOf[Unused])
  
  private def isInj[T](l: Seq[T]): Boolean = 
    l.distinct == l
    
  private implicit def injectAt(l: List[RenamedNode]) = new {
    def at(i: Int): RenamedNode =
      if(i < 0 || i >= l.size)
        add(Unused(), Nil)
      else
        l(i)
  }
  
  def applyTransformation(
        trans: PartialFunction[(Hyperedge, Hyperedge), Unit], 
        h1o: Hyperedge, h2o: Hyperedge): Boolean = {
    var done = false
    val tlifted = trans.lift
    for((h1,h2) <- transformablePairs(normalize(h1o), normalize(h2o)))
      if(tlifted((h1,h2)).isDefined)
        done |= true
    done
  }
  
  def transDrive =
    letVar & letLet & letCaseOf & letOther & caseVar & caseCase & caseTick
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
    
  // let x = e in x  ->  e
  def letVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(Let(), src1, f1 :: es1),
          h2@Hyperedge(Var(), src2, List())) if f1.plain == src2 =>
      trans("letVar", h1, h2) {
        val varnum = f1.renaming(0)
        add(Id(), src1, List(es1 at varnum))
      }
  }
  
  def letLet: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(Let(), src1, f1 :: es1),
          h2@Hyperedge(Let(), src2, f2 :: es2)) if f1.plain == src2 =>
      trans("letLet", h1, h2) {
        val newes = es2.map(e => add(Let(), (f1.renaming comp e) :: es1))
        add(Let(), src1, f2 :: newes)
      }
  }
  
  def letCaseOf: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(Let(), src1, f :: es),
          h2@Hyperedge(CaseOf(cases), src2, g :: hs)) if f.plain == src2 =>
      if(!f.isPlain)
        letCaseOf((
          Hyperedge(Let(), src1, f.plain :: es),
          Hyperedge(h2.label, src2, f.renaming compDests h2)))
      else {
        trans("letCaseOf", h1, h2) {
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
  }
  
  // Construct, Id, Tick, Improvement...
  def letOther: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(Let(), src1, f :: es),
          h2@Hyperedge(l, src2, gs)) if f.plain == src2 && l.isSimple && gs.nonEmpty =>
      trans("letOther", h1, h2) {
        add(l, src1, gs.map(g => add(Let(), (f.renaming comp g) :: es)))
      }
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  // This transformation is performed automatically during normalization, don't use it
  def caseReduce: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases), src1, e :: hs),
          h2@Hyperedge(Construct(name), src2, args)) if e.plain == src2 =>
      trans("caseReduce", h1, h2) {
        val ((_,n),h) = (cases zip hs).find(_._1._1 == name).get
        assert(n == args.size)
        val bs = 
          args.map(e.renaming comp _) ++ 
          (n until h.arity).map(i => variable(i - n))
        
        add(Let(), src1, List(h) ++ bs)
      }
  }
  
  // propagate positive information
  def caseVar: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases), src1, e :: hs),
          h2@Hyperedge(Var(), src2, Nil)) if e.plain == src2 =>
      trans("caseVar", h1, h2) {
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
  }
  
  def caseCase: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(CaseOf(cases2), src2, e2 :: fs2)) if e1.plain == src2 =>
      if(!e1.isPlain)
        caseCase((
          Hyperedge(CaseOf(cases1), src1, e1.plain :: fs1),
          Hyperedge(h2.label, src2, e1.renaming compDests h2)))
      else {
        trans("caseCase", h1, h2) {
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
  }
  
  // You might think that there will be a problem if src2 is a dest node of h1 several times
  // but TransformManager replaces the node between the hyperedges with a dummy node, so 
  // there is no problem
  def caseTick: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(Tick(), src2, List(e2))) if e1.plain == src2 =>
      trans("caseTick", h1, h2) {
        val caseof = add(CaseOf(cases1), (e1.renaming comp e2) :: fs1)
        add(Tick(), src1, List(caseof))
      }
    case (h1@Hyperedge(CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(Tick(), src2, List(e2))) if 
            fs1.exists(_.plain == src2) && 
            fs1.forall(f => f.plain == src2 || f.node.outs.exists(_.label.isInstanceOf[Tick])) =>
      trans("caseTick", h1, h2) {
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
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  // Move the let up through let, a special case of letUp
  def letLetUp(maxarity: Int = Int.MaxValue): PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(Let(), src1, es1),
          h2@Hyperedge(Let(), src2, f2 :: es2)) if 
            es1.tail.exists(_.plain == src2) =>
      letUp(maxarity)((h1,h2))
  }
  
  // Move the let up, i.e. generalize
  def letUp(maxarity: Int = Int.MaxValue): PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(l1, src1, es1),
          h2@Hyperedge(Let(), src2, f2 :: es2)) if 
            (if(l1 == Let()) es1.tail else es1).exists(_.plain == src2) =>
      val (e1, shift) = (es1 zip h1.shifts).find(_._1.plain == src2).get
      // if it is a head of a let then you've found a bug, congratulations
      assert(shift != -1)
      
      var arity = h1.arity
      var extexprs = (0 until arity).map(variable(_)).toList
      
      val newes2 =
        for(e2 <- es2) yield {
          val e2prime = e1.renaming comp e2
          if(e2prime.used.forall(_ >= shift)) {
            extexprs ++= 
              List(e2prime.renaming.mapVars(_ - shift) comp e2.node)
            var vrbl = variable(arity + shift)
            arity += 1
            vrbl
          } else {
            // we cannot move this expression up because it uses bound variables
            e2prime
          }
        }
      
      // We need these dummy hyperedges to compute the real arity of the result
      val newlet_h = Hyperedge(Let(), null, f2 :: newes2)
      val newl1_h = Hyperedge(l1, null, es1.map(e => if(e == e1) newlet_h.asDummyNode else e))
      
      if(newl1_h.used.size <= maxarity) {
        trans("letUp", h1, h2) {
          val newlet = add(Let(), f2 :: newes2)
          val newl1 = add(l1, es1.map(e => if(e == e1) newlet else e))
          add(Let(), src1, newl1 :: extexprs)
        }
      }
  }
  
  // case e of {...}  ->  let x = e in case x of {...}
  def caseGen: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases), src1, es1), h2) if 
            es1(0).deref.getVar.isEmpty =>
      trans("caseGen", h1, h2) {
        var arity = h1.arity
        var extexprs = (0 until arity).map(variable(_)).toList
        val newcaseof = add(CaseOf(cases), variable(arity) :: es1.tail)
        add(Let(), src1, newcaseof :: extexprs ++ List(es1(0)))
      }
  }
  
  // f(x,x)  ->  let y = x, z = x in f(y,z)
  def unshare(maxarity: Int = Int.MaxValue): PartialFunction[Hyperedge, Unit] = {
    case h if(h.used.size + 1 <= maxarity) =>
      trans("unshare", h) {
        for((m, shift) <- (0 until h.dests.length) zip h.shifts if shift != -1) {
          val (init, d :: tail) = h.dests.splitAt(m)
          for(v <- d.used if v >= shift) {
            val newd = d.renaming.mapVars(i => if(i == v) h.arity + shift else i) comp d.node
            val newsrc = add(h.label, init ++ (newd :: tail))
            if(!(newsrc ~~ h.source)) {
              add(Let(), h.source, 
                  newsrc :: (0 until h.arity).map(variable(_)).toList ++ List(variable(v - shift)))
            }
          }
        }
      }
  }
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  
  // Don't use this function together with buffering! It relies on 
  // immediate appearance of added hyperedges.
  final def drive(n: Node, hist: List[Node] = Nil): Option[Hyperedge] = {
    val node = n.deref.node
    node.definingHyperedge match {
      case Some(h) => Some(h)
      case None =>
        if(hist.exists(_.deref.node == node))
          return None
        
        val caseofs = node.outs.filter(_.label.isInstanceOf[CaseOf]).toList
        if(caseofs.nonEmpty) {
          var changed = false
          for(c <- caseofs)
            drive(c.dests(0).node, n :: hist) match {
              case Some(childdef) =>
                if(childdef.label.isInstanceOf[CaseOf])
                  applyTransformation(caseCase, c, childdef)
                else if(childdef.label.isInstanceOf[Var])
                  applyTransformation(caseVar, c, childdef)
                else if(childdef.label.isInstanceOf[Tick])
                  applyTransformation(caseTick, c, childdef)
                changed = true
              case None =>
            }
          
          if(changed)
            drive(n, n :: hist)
          else
            None
        }
        else {
          var changed = false
          for(l <- node.outs) {
            assert(l.label == Let())
            drive(l.dests(0).node, n :: hist) match {
              case Some(childdef) =>
                if(childdef.label.isInstanceOf[CaseOf])
                  applyTransformation(letCaseOf, l, childdef)
                else if(childdef.label.isInstanceOf[Var])
                  applyTransformation(letVar, l, childdef)
                else if(childdef.label.isInstanceOf[Let])
                  applyTransformation(letLet, l, childdef)
                else
                  applyTransformation(letOther, l, childdef)
                changed = true
              case None =>
            }
          }
          
          if(changed)
            drive(n, hist)
          else
            None
        }
    }
  }
  
}


