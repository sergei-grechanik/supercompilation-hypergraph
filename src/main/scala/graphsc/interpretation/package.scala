package graphsc

package object interpretation {
  type TrieSubst = Map[TrieVar, TrieVal]
  
  def runHyperedge(
      h: Hyperedge, 
      args: List[Value], 
      nodeRunner: (RenamedNode, List[Value]) => Value,
      total: Boolean = false): Value = {
    val Hyperedge(label, source, dests) = h
    label match {
      case Construct(name) =>
        // Bottoms are like ordinary bottoms, ErrorBottoms propagate through constructors
        val subs = dests.map(nodeRunner(_, args))
        if(subs.contains(ErrorBottom))
          ErrorBottom
        else
          Ctr(name, subs)
      case CaseOf(cases) =>
        nodeRunner(dests(0), args) match {
          case Ctr(cname, cargs) =>
            val Some(((_, n), expr)) = (cases zip dests.tail).find(_._1._1 == cname)
            assert(cargs.size == n)
            nodeRunner(expr, cargs ++ args)
          case Bottom if !total => Bottom
          case Bottom if total =>
//            val ress =
//              for(((_,n), expr) <- cases zip dests.tail) yield
//                nodeRunner(expr, List.fill(n)(Bottom) ++ args)
//            (ress(0) /: ress.tail)(_ & _)
            ErrorBottom
          case ErrorBottom => ErrorBottom
        }
      case Let() =>
        val newargs = dests.tail.map(nodeRunner(_, args))
        nodeRunner(dests(0), newargs)
      case Tick() =>
        nodeRunner(dests(0), args)
      case Improvement() =>
        nodeRunner(dests(0), args)
      case Id() => 
        nodeRunner(dests(0), args)
      case Var() =>
        args(0)
      case Unused() =>
        Bottom
    }
  }
  
  def runHyperedgeAndStuff(
      h: Hyperedge, args: List[Value], 
      nodeRunner: (RenamedNode, List[Value]) => ValueAndStuff, 
      total: Boolean = false): ValueAndStuff = {
    val Hyperedge(label, source, dests) = h
    label match {
      case Construct(name) =>
        // Bottoms are like ordinary bottoms, ErrorBottoms propagate through constructors
        def subs = dests.map(nodeRunner(_, args))
        if(subs.contains(ErrorBottom))
          ValueAndStuff(ErrorBottom, 0, Nil)
        else
          ValueAndStuff(Ctr(name, subs.map(_.value)), subs.map(_.cost).sum + 1, List(h))
      case CaseOf(cases) =>
        val what = nodeRunner(dests(0), args)
        what.value match {
          case Ctr(cname, cargs) =>
            val Some(((_, n), expr)) = (cases zip dests.tail).find(_._1._1 == cname)
            assert(cargs.size == n)
            val res = nodeRunner(expr, cargs ++ args)
            ValueAndStuff(res.value, what.cost + res.cost + 1, List(h))
          case Bottom if !total => ValueAndStuff(Bottom, 1, List(h))
          case Bottom if total =>
            val ress =
              for(((_,n), expr) <- cases zip dests.tail) yield
                nodeRunner(expr, List.fill(n)(Bottom) ++ args)
            (ress(0) /: ress.tail)(_ & _)
          case ErrorBottom => ValueAndStuff(ErrorBottom, 0, Nil)
        }
      case Let() =>
        val newargs = dests.tail.map(nodeRunner(_, args))
        val res = nodeRunner(dests(0), newargs.map(_.value))
        ValueAndStuff(res.value, newargs.map(_.cost).sum + res.cost + 1, List(h))
      case Tick() =>
        val res = nodeRunner(dests(0), args)
        ValueAndStuff(res.value, res.cost + 1, List(h))
      case Improvement() =>
        val res = nodeRunner(dests(0), args)
        ValueAndStuff(res.value, res.cost + 1, List(h))
      case Id() => 
        val res = nodeRunner(dests(0), args)
        ValueAndStuff(res.value, res.cost + 1, List(h))
      case Var() =>
        ValueAndStuff(args(0), 1, List(h))
      case Unused() =>
        ValueAndStuff(Bottom, 0, List(h))
    }
  }
}