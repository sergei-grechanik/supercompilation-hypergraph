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
  
  def applyTransformation(
        trans: (Hyperedge, Hyperedge) => Boolean, 
        h1o: Hyperedge, h2o: Hyperedge): Boolean = {
    var done = false
    for((h1,h2) <- transformablePairs(normalize(h1o), normalize(h2o)))
      done |= trans(h1,h2)
    done
  }
  
  def transDrive =
    anyId & letLet & letCaseOf & letOther & caseVar & caseCase & caseTick
  
  def transNone = BFun2BiHProc(Nil)
  
  
  def transTotal = transDrive & caseConstrTotal & caseCaseSwap(true)
  def transUntotal = transDrive & caseCaseSwap(false)
  
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  def anyId: (Hyperedge, Hyperedge) => Boolean = {
    case (h1@Hyperedge(l1, src1, es1),
          h2@Hyperedge(Id(), src2, List(e2))) =>
      trans("anyId", h1, h2) {
        add(l1, src1, es1.map(e => if(e.plain == src2) e.renaming comp e2 else e))
      }
      true
    case _ => false
  }
  
  // let x = e in x  ->  e
  def letVar: (Hyperedge, Hyperedge) => Boolean = {
    case (h1@Hyperedge(Let(), src1, f1 :: es1),
          h2@Hyperedge(Var(), src2, List())) if f1.plain == src2 =>
      trans("letVar", h1, h2) {
        val varnum = f1.renaming(0)
        add(Id(), src1, List(es1 at varnum))
      }
      true
    case _ => false
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
  
  def letToId: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(Let(), src1, f1 :: es1),
          h2@Hyperedge(Var(), src2, List())) if 
            f1.plain != src2 && es1.forall(_.deref.getVar.isDefined) =>
      val newhead = f1.plain
      val newtail = 
        (0 until newhead.arity toList).map { i =>
          f1.renaming(i) match {
            case j if j < 0 || j >= es1.size => unused
            case j => es1(j)
          }
        }
      
      lazy val vec = newtail.map(_.deref.getVarUnused.get)
      lazy val ren = Renaming(vec).normal
      lazy val renhead = ren comp newhead
      if(newtail.forall(_.deref.getVarUnused.isDefined) && 
         vec.filter(_ >= 0).distinct.size == vec.filter(_ >= 0).size &&
         renhead.isInvertible) {
        trans("letToId", h1, h2) {
          add(Id(), src1, List(renhead))
        }
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
  
  // TODO: Many of these transformations are ternary!
  
  // factoring out caseofs from branches (in any setting)
  // case e of { A -> case x of {..}; B -> case x of {...} }  ->  case x of {...}
  def caseCaseSwap(total: Boolean = false): PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(l1@CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(l2@CaseOf(cases2), src2, e2 :: fs2)) if 
            fs1.exists(_.plain == src2) &&
            e2.getVar.nonEmpty &&
            varForCaseCaseTotal(h1, h2) != -1 &&
            (total ||
            fs1.forall(f => f.plain == src2 || 
                f.node.outs.exists(o => o.label == l2 && o.dests.size == h2.dests.size))) =>
      val vari = varForCaseCaseTotal(h1, h2)
      // say, h1 and h2 are such that:
      // h1: src1(x) = case e(x) of { A yA -> (rA fsA)(yA,x); B yB -> (rB fsB)(yB,x) }
      // h2 = hA: fsA(y,x) = case x of { C z -> fsAC(z,y,x); D z -> fsAD(z,y,x) }
      // h3 = hB: fsB(y,x) = case x of { C z -> fsBC(z,y,x); D z -> fsBD(z,y,x) }
      trans("caseCaseSwap", h1, h2) {
        // newfs1list = [[(hA, rA, |yA|), (hB, rB, |yB|)]]
        val newfs1list =
          sequence((fs1 zip h1.shifts.tail).map { case (n,sh) =>
            if(n.plain == src2) 
              List((h2, n.renaming, sh))
            else
              maybeAddDummy(total, n, cases2, sh, vari + sh,
                n.node.outs.toList.filter(o => 
                    o.label == l2 && 
                    o.dests.size == h2.dests.size &&
                    o.dests(0).getVar.map((n.renaming comp o.source.renaming.inv)(_)) == 
                      Some(vari + sh) )
                  .map(o => (o, n.renaming comp o.source.renaming.inv, sh)))
          })
          
        for(l <- newfs1list) trans("subtrans of caseCaseSwap", l.map(_._1):_*) {
          // l = [(hA, rA, |yA|), (hB, rB, |yB|)]
          val dests =
            for((h,r,s1) <- l) yield {
              // say, (hA, rA, |yA|)
              for((d,s2) <- (h.dests zip h.shifts).tail) yield {
                // (d,s2) <- [(fsAC, |z|), (fsAD, |z|)]
                // say, d = fsAC(z, y, x)
                d.renaming.mapVars(i =>
                  if(i < s2) i + s1 // i \in z
                  else {
                    // i \in (y, x)
                    val j = r(i - s2) // j \in rA (y, x) = (y',x')
                    if(j < s1) j // j \in y'
                    else (j - s1) + s1 + s2 //j \in x' 
                  }
                ) comp d.node
                // d = fsCA(y', z, x') = fsAC(z, y, x)
              }
            }
          // dests = [[fsCA(y', z, x'), fsDA], [fsCB, fsDB]]
        
          val caseofs = 
            for((ds,s2) <- dests.transpose zip h2.shifts.tail) 
              // say, ds = [fsCA, fsCB], s2 = |z|
              // case e(x) of { A y -> fsCA(y,z,x); B y -> fsCB(y,z,x) }
              yield add(CaseOf(cases1), (e1.renaming.mapVars(_ + s2) comp e1.node) :: ds)
        
          add(CaseOf(cases2), src1, variable(vari) :: caseofs)
        }
      }
  }
  
  private def varForCaseCaseTotal(h1: Hyperedge, h2: Hyperedge): Int = {
    val Hyperedge(CaseOf(cases1), src1, e1 :: fs1) = h1
    val Hyperedge(l2@CaseOf(cases2), src2, e2 :: es2) = h2
    val e2var = e2.getVar.get
    val (f1,s) = (fs1 zip h1.shifts.tail).find(_._1.plain == src2).get
    val f1var = f1.renaming(e2var)
    if(f1var < s)
      -1
    else
      f1var - s
  }
  
  // Add a dummy caseof if hs is empty (only if total)
  def maybeAddDummy(total: Boolean, n: RenamedNode, cases: List[(String, Int)], 
                    shift: Int, v: Int, hs: List[(Hyperedge, Renaming, Int)]): 
                        List[(Hyperedge, Renaming, Int)] = hs match {
    case Nil if total =>
      val h = Hyperedge(CaseOf(cases), (new FreeNode(Set())).deref, 
          variable(v) :: cases.map{ case (_,sh) => n.renaming.mapVars(_ + sh) comp n.node })
      List((h, Renaming(h.used), shift))
    case _ => hs
  }
  
  // factoring out constructors from branches (in total setting)
  // case e of { A -> S e1; B -> S e2 }  ->  S (case e of { A -> e1; B -> e2 })
  def caseConstrTotal: PartialFunction[(Hyperedge, Hyperedge), Unit] = {
    case (h1@Hyperedge(CaseOf(cases1), src1, e1 :: fs1),
          h2@Hyperedge(l2@Construct(name), src2, es2)) if 
            fs1.exists(_.plain == src2) && 
            fs1.forall(f => f.plain == src2 || 
                f.node.outs.exists(o => o.label == l2 && o.dests.size == es2.size)) =>
      trans("caseConstrTotal", h1, h2) {
        val newfs1list =
          sequence(fs1.map(n =>
            if(n.plain == src2) 
              List((h2, n.renaming))
            else
              n.node.outs.toList.filter(o => o.label == l2 && o.dests.size == es2.size)
                .map((_, n.renaming))
          ))
        for(l <- newfs1list) {
          val caseofs = 
            for(ds <- l.map{case (h,r) => (r comp h.source.renaming.inv comp h).dests}.transpose) 
              yield add(CaseOf(cases1), e1 :: ds)
          add(Construct(name), src1, caseofs)
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


