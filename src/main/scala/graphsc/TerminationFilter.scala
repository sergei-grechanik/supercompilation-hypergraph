package graphsc

sealed trait ValueDescription {
  def less(v: ValueDescription): Boolean
  def access(i: Int): ValueDescription = this match {
    case VDCtr(as) =>
      if(i >= 0 && i < as.size)
        as(i)
      else
        VDCtr(Nil)
    case VDVar(j, subs) => VDVar(j, subs ++ List(i))
    case VDUnknown() => VDUnknown()
  }
}

case class VDCtr(args: List[ValueDescription]) extends ValueDescription {
  override def less(v: ValueDescription): Boolean = v match {
    case VDCtr(as) =>
      val bycoupling =
        if(args.size < as.size)
          true
        else if(args.size > as.size)
          false
        else
          (args zip as).exists { case (a,b) => a less b }
      bycoupling || as.exists(a => this == a || (this less a))
    case VDVar(_, _) => false
    case VDUnknown() => false
  }
}

case class VDVar(i: Int, subobject: List[Int]) extends ValueDescription {
  override def less(v: ValueDescription): Boolean = v match {
    case VDCtr(as) =>
      as.exists(a => this == a || (this less a))
    case VDVar(j, sub) =>
      i == j && sub.size < subobject.size && subobject.take(sub.size) == sub
    case VDUnknown() => false
  }
}

case class VDUnknown() extends ValueDescription {
  override def less(v: ValueDescription): Boolean = false
}

class TerminationFilter(graph: Hypergraph) {
  
  private case class Safety(
      args: List[ValueDescription],
      origargs: List[ValueDescription],
      preguarded: Boolean = true, 
      guarded: Boolean = false) {
    
    def safe: Boolean =
      (VDCtr(args) less VDCtr(origargs)) || guarded
    
    def rename(r: Renaming, as: List[ValueDescription]): List[ValueDescription] = {
      r.vector.map {
        i =>
          if(i >= 0 && i < as.size)
            as(i)
          else
            VDCtr(Nil)
      }
    }
    
    def expand(
        v: ValueDescription, 
        expanded: ValueDescription, 
        where: ValueDescription): ValueDescription = v match {
      case VDVar(i, l) =>
        if(v == where)
          expanded
        else
          where match {
            case VDCtr(lst) => VDCtr(lst.map(expand(v, expanded, _)))
            case _ => where
          }
      case _ => where
    }
    
    def run(n: RenamedNode, as: List[ValueDescription], h: List[Node] = Nil): ValueDescription = {
      if(h.contains(n.node))
        VDUnknown()
      else
        n.getVar match {
          case Some(i) =>
            if(i >= 0 && i < as.size) as(i) else VDCtr(Nil)
          case None =>
            (n.node.outs.find(_.label.isInstanceOf[Construct]) : @unchecked) match {
              case Some(Hyperedge(Construct(_), src, ds)) =>
                val as1 = rename(src.renaming.inv, as)
                VDCtr(ds.map(d => run(d, as1, n.node :: h)))
              case None => VDUnknown()
            }
        }
    }
    
    def through(h: Hyperedge): List[Safety] = {
      val args1 = rename(h.source.renaming.inv, args)
      h.label match {
        case Construct(_) =>
          h.dests.map {
            d =>
              // If the position was preguarded then it is now guarded
              Safety(rename(d.renaming, args1), origargs, preguarded, preguarded)
          }
        case CaseOf(cases) =>
          Safety(rename(h.dests(0).renaming, args1), origargs, false, false) ::
            (h.dests(0).getVar match {
              case Some(i) =>
                val varvd = if(i >= 0 && i < args1.size) args1(i) else VDCtr(Nil)
                (h.dests.tail zip cases).map {
                  case (d,(_,n)) =>
                    val vars = (0 until n).map(varvd access _).toList
                    val newargs = vars ++ args1.map(expand(varvd, VDCtr(vars), _))
                    val neworigargs = origargs.map(expand(varvd, VDCtr(vars), _))
                    Safety(rename(d.renaming, newargs), neworigargs, preguarded, guarded)
                }
              case None =>
                (h.dests.tail zip cases).map {
                  case (d,(_,n)) =>
                    val newargs = List.fill(n)(VDUnknown()) ++ args1
                    Safety(rename(d.renaming, newargs), origargs, preguarded, guarded)
                }
            })
        case Let() =>
          val newargs = h.dests.tail.map(run(_, args1))
          Safety(rename(h.dests(0).renaming, newargs), origargs, preguarded, guarded) ::
            h.dests.tail.map {
              d =>
                Safety(rename(d.renaming, args1), origargs, false, false)
            }
        case Tick() =>
          List(Safety(rename(h.dests(0).renaming, args1), origargs, preguarded, preguarded))
        case Improvement() =>
          List(Safety(rename(h.dests(0).renaming, args1), origargs, preguarded, guarded))
        case Id() => 
          List(Safety(rename(h.dests(0).renaming, args1), origargs, preguarded, guarded))
        case Var() =>
          Nil
        case Error() =>
          Nil
      }
    }
  }
  
  private object Safety {
    def apply(n: RenamedNode): Safety =
      Safety(
          (0 until n.arity).map(i => if(n.used(i)) VDVar(i, Nil) else VDCtr(Nil)).toList,
          (0 until n.arity).toList.map(VDVar(_, Nil)),
          true, false)
  }
   
  def apply(n: RenamedNode): Option[RenamedNode] =
    go(n.node, Nil).map(n.renaming comp _._1)
  
  private def go(n: Node, hist: List[(Node, Safety)]): 
      Option[(RenamedNode, List[(Node, RenamedNode)])] = {
    hist.find(_._1 == n) match {
      case Some((_, s)) if s.safe =>
        val node = graph.newNode(n.used)
        Some((node, List(n -> node)))
      case Some((_, s)) =>
        None
      case None =>
        val newnode = graph.newNode(n.used)
        val maps =
          n.outs.toList.map {
            h =>
              val histnodes = n :: hist.map(_._1)
              val newsafeties = (Safety(n.deref) :: hist.map(_._2)).map(_.through(h)).transpose
              val children = 
                h.dests.zip(newsafeties).map { case (d,ss) => go(d.node, histnodes zip ss) }
              if(children.forall(_.isDefined)) {
                val newdests = 
                  children.map(_.get._1).zip(h.dests).map { case (d,old) => old.renaming comp d }
                val maps = children.map(_.get._2)
                graph.add(h.label, h.source.renaming comp newnode.deref, newdests)
                maps
              } else
                Nil
          } flatten
          
        if(newnode.deref.node.outs.nonEmpty) {
          val resmap = 
            (List(n -> newnode.deref) :: maps)
              .flatten.groupBy(_._1).mapValues(l => graph.glue(l.map(_._2)))
          assert(resmap.map(_._1).toSet subsetOf (n :: hist.map(_._1)).toSet)
          Some(resmap(n), resmap.toList.filter(_._1 != n))
        }
        else
          None
    }
  }
}