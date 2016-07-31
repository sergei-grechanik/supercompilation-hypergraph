package graphsc
package interpretation

sealed trait Trie {
  def process( 
        f: (TrieSubst, TrieVal) => Trie,
        contr: TrieSubst = Map()): Trie = this match {
    case t:TrieThunk => TrieThunkFun(() => t.unroll.process(f, contr)) 
    case t:TrieVar => f(contr, t)
    case TrieBottom => f(contr, TrieBottom)
    case t:TrieConstr => f(contr, t)
    case TrieCaseOf(v:TrieVar, cs) =>
      if(contr.contains(v))
        throw new Exception("Trie is not perfect: a variable is scrutinized twice")
      TrieCaseOf(v, cs.map{
        case (n, k, t) =>
          val c = Map(v -> v.split(n, k))
          (n, k, t.process(f, contr.mapValues(_.subst(c)) ++ c))
      })
  }
  
  def subst(s: TrieSubst): Trie = this match {
    case t:TrieThunk => TrieThunkSubst(t, s)
    case t:TrieVal => t.subst(s)
    case TrieCaseOf(v:TrieVar, cs) =>
      s.get(v) match {
        case None =>
          TrieCaseOf(v, cs.map{
            case (n, k, t) =>
              val c = Map(v -> v.split(n, k))
              (n, k, t.subst(s.mapValues(_.subst(c))))
          })
        case Some(TrieBottom) => TrieBottom
        case Some(v1:TrieVar) =>
          TrieCaseOf(v, cs.map{
            case (n, k, t) =>
              val c = Map(v1 -> v.split(n, k))
              (n, k, t.subst(s.mapValues(_.subst(c)) ++ c))
          })
        case Some(TrieConstr(n, as)) =>
          cs.find(x => x._1 == n && x._2 == as.size) match {
            case None => TrieBottom
            case Some((_, k, t)) =>
              val c = 
                (0 until k).map(i => 
                  TrieVar(v.path ++ List(i)) -> as(i).asInstanceOf[TrieVal]).toMap
              t.subst(s.mapValues(_.subst(c)) ++ c)
          }
      }
  }
  
  def get: Trie = this match {
    case t:TrieThunk => t.unrolled
    case _ => this
  }
  
  def toValue: Value = this match {
    case TrieVar(_) => Bottom
    case TrieConstr(cn, as) => Ctr(cn, as.map(_.toValue)).optimize
    case TrieBottom => Bottom
    case t:TrieThunk => t.unrolled.toValue
    case TrieCaseOf(_, _) => throw new Exception("Cannot convert a caseof to value")
  }
  
  def dump(depth: Int, maxthunks: Int = 20): String = this match {
    case _ if depth <= 0 => "..."
    case TrieBottom => "_|_"
    case TrieVar(p) => "v" + p.mkString(".") 
    case t:TrieThunk =>
      def go(t: Trie, i: Int): String = t match {
        case t:TrieThunk if i > 0 => go(t.unroll, i - 1)
        case t:TrieThunk => "[too many thunks]"
        case _ => t.dump(depth, maxthunks)
      }
      go(t, maxthunks)
    case TrieConstr(cname, Nil) => cname
    case TrieConstr(cname, as) =>
      cname + "\n" + as.map(a => "|-" + indent1(a.dump(depth - 1, maxthunks), "| ")).mkString("\n")
    case TrieCaseOf(v, cs) =>
      val varname = v.dump(depth, maxthunks)
      "case " + varname + " of\n" + cs.map{
        case (cn, k, t) =>
          "|-" + varname + " = " + cn + " " + 
          v.split(cn, k).args.map(_.dump(depth, maxthunks)).mkString(" ") + "\n" +
          indent(t.dump(depth - 1, maxthunks), "|  ")
      }.sortBy(_.count(_ == '\n')).mkString("\n")
  }
}

case class TrieCaseOf(variable: TrieVar, cases: List[(String, Int, Trie)]) extends Trie

sealed trait TrieVal extends Trie {
  override def subst(s: TrieSubst): TrieVal = this match {
    case TrieBottom => TrieBottom
    case v:TrieVar => s.getOrElse(v, v)
    case TrieConstr(n, as) => TrieConstr(n, as.map(_.subst(s)))
  }
}

case class TrieConstr(name: String, args: List[Trie]) extends TrieVal

case object TrieBottom extends TrieVal

case class TrieVar(path: List[Int]) extends TrieVal {
  def split(cname: String, k: Int): TrieConstr =
    TrieConstr(cname, (0 until k).map(i => TrieVar(path ++ List(i))).toList)
}


trait TrieThunk extends Trie {
  protected def unrollImpl: Trie
  
  lazy val unroll: Trie = unrollImpl
  
  def unrollMore(n: Int): Trie =
    if(n == 0) this
    else unroll match {
      case t:TrieThunk => t.unrollMore(n - 1)
      case t => t
    }
  
  lazy val unrolled: Trie = unroll match {
    case t:TrieThunk => t.unrolled
    case t => t
  }
}

case class TrieThunkFun(f: () => Trie) extends TrieThunk {
  override def unrollImpl = f()
}

case class TrieThunkSubst(t: TrieThunk, s: TrieSubst) extends TrieThunk {
  override def unrollImpl = t.unroll.subst(s)
}


object Trie {
  case class TrieThunkNode(n: Node, args: List[Trie]) extends TrieThunk {
    override def unrollImpl = {
      val prelst =
        n.outs.sortBy(h => h.label match {
          case _ if isDefining(h) => (0,0)
          case Let() => (3,-h.dests.size)
          case CaseOf(_) => (2,0)
          case _ => (1,0)
        })
      val lst = prelst.takeWhile(_.label == prelst(0).label).map(h =>
          mkTrie(h, h.source.renaming.inv.vector.map(i => 
            if(i == -1) TrieBottom else args(i))))
      waitForAny(lst)
    }
  }
  
  case class TrieThunkWaitForAny(lst: List[TrieThunk]) extends TrieThunk {
    override def unrollImpl = waitForAny(lst.map(_.unroll))
  }
  
  val trieCache = collection.mutable.Map[(Node, List[Trie]), Trie]()
  
  def mkTrie(n: RenamedNode): Trie =
    mkTrie(n, (0 until n.arity).map(i => TrieVar(List(i))).toList)
  
  def mkTrie(n: RenamedNode, args: List[Trie]): Trie = {
    val key = (n.node, n.renaming.vector.map(i => 
                          if(i == -1) TrieBottom else args(i)))
    trieCache.getOrElseUpdate(key, TrieThunkNode(key._1, key._2))
  }
  
  def waitForAny(lst: List[Trie]): Trie = {
    lst.find(!_.isInstanceOf[TrieThunk]) match {
      case Some(t) => t
      case None => TrieThunkWaitForAny(lst.map(_.asInstanceOf[TrieThunk]))
    }
  }
  
  def mkTrie(h: Hyperedge, args: List[Trie]): Trie = {
    val Hyperedge(label, source, dests) = h
    label match {
      case Construct(name) =>
        TrieConstr(name, dests.map(mkTrie(_, args)))
      case CaseOf(cases) =>
        mkTrie(dests(0), args).process{
          case (s, TrieConstr(cname, cargs)) =>
            (cases zip dests.tail).find(x => x._1._1 == cname && x._1._2 == cargs.size) match {
              case None => TrieBottom
              case Some(((_, n), expr)) =>
                mkTrie(expr, cargs ++ args.map(_.subst(s)))
            }
          case (_, TrieBottom) => TrieBottom
          case (s, v:TrieVar) =>
            TrieCaseOf(v, (cases zip dests.tail).map{
              case ((cn, k), expr) =>
                val spl = v.split(cn, k)
                (cn, k, mkTrie(expr, spl.args ++ args.map(_.subst(s + (v -> spl)))))
            })
        }
      case Let() =>
        val newargs = dests.tail.map(mkTrie(_, args))
        mkTrie(dests(0), newargs)
      case Tick() =>
        mkTrie(dests(0), args)
      case Improvement() =>
        mkTrie(dests(0), args)
      case Id() => 
        mkTrie(dests(0), args)
      case Var() =>
        args(0)
      case Unused() =>
        TrieBottom
    }
  }
}
