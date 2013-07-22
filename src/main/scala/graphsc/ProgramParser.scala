package graphsc

import scala.util.parsing.combinator._
import graphsc.interpretation._
import java.io.File

//sealed trait Expr
case class ExprLambda(vars: List[String], body: Expr) extends Expr
case class ExprFun(name: String) extends Expr
case class ExprVar(name: String) extends Expr
case class ExprConstr(name: String) extends Expr
case class ExprUnused() extends Expr
case class ExprCall(fun: Expr, args: List[Expr]) extends Expr
case class ExprCaseOf(expr: Expr, cases: List[(String, List[String], Expr)]) extends Expr
case class ExprLet(expr: Expr, binds: List[(String, Expr)]) extends Expr

sealed trait GoalProp
case class GoalPropEq(left: RenamedNode, right: RenamedNode) extends GoalProp
case class GoalPropEqModuloRen(left: RenamedNode, right: RenamedNode) extends GoalProp
case class GoalPropReturnsConstr(node: RenamedNode, constr: String) extends GoalProp

sealed trait Prop {
  def mapExprs(f: Expr => Expr): Prop
  
  def allExprs: List[Expr] = this match {
    case PropEq(l, r) => List(l, r)
    case PropEqModuloRen(l, r) => List(l, r)
    case PropReturnsConstr(e, _) => List(e)
    case PropNamed(_) => Nil
  }
  
  def loadInto(g: NamedNodes) = this match {
    case PropEq(lhs, rhs) =>
      val free = (lhs.freeVars ++ rhs.freeVars).toList
      val l = ExprLambda(free, lhs).loadInto(g)
      val r = ExprLambda(free, rhs).loadInto(g)
      g.glue(List(l, r))
    case _ =>
      throw new Exception("Loading props other than those stating equivalence is not supported")
  }
  
  def loadExprsInto(g: NamedNodes): List[RenamedNode] = {
    val free = allExprs.map(_.freeVars).flatten.distinct
    for(e <- allExprs) yield
      ExprLambda(free, e).loadInto(g)
  }
  
  def loadAsGoal(g: NamedNodes): GoalProp = {
    val lst = loadExprsInto(g)
    this match {
      case PropEq(lhs, rhs) =>
        GoalPropEq(lst(0), lst(1))
      case PropEqModuloRen(lhs, rhs) =>
        GoalPropEqModuloRen(lst(0), lst(1))
      case PropReturnsConstr(lhs, c) =>
        GoalPropReturnsConstr(lst(0), c)
      case PropNamed(_) =>
        throw new Exception("Named props are not supported yet")
    }
  }
}

case class PropEq(left: Expr, right: Expr) extends Prop {
  def mapExprs(f: Expr => Expr): PropEq = PropEq(f(left), f(right))
}

case class PropEqModuloRen(left: Expr, right: Expr) extends Prop {
  def mapExprs(f: Expr => Expr): PropEqModuloRen = PropEqModuloRen(f(left), f(right))
}

case class PropReturnsConstr(expr: Expr, constr: String) extends Prop {
  def mapExprs(f: Expr => Expr): PropReturnsConstr = PropReturnsConstr(f(expr), constr)
}

case class PropNamed(name: String) extends Prop {
  def mapExprs(f: Expr => Expr): PropNamed = this
}

sealed trait Expr {
  def arity = this match {
    case ExprLambda(vs, _) => vs.size
    case _ => 0
  }
  
  def mapChildren(f: (List[String], Expr) => Expr): Expr = this match {
    case ExprLambda(vars, body) => ExprLambda(vars, f(vars, body))
    case ExprCall(fun, args) => ExprCall(f(Nil, fun), args.map(f(Nil, _)))
    case ExprCaseOf(expr, cases) =>
      ExprCaseOf(f(Nil, expr), cases.map{ case (c, vs, b) => (c, vs, f(vs, b)) })
    case ExprLet(expr, binds) =>
      ExprLet(f(binds.map(_._1), expr), binds.map{ case (v, b) => (v, f(Nil, b)) })
    case _ => this
  }
  
  def mapChildrenToList[B](f: (List[String], Expr) => B): List[B] = this match {
    case ExprLambda(vars, body) => List(f(vars, body))
    case ExprCall(fun, args) => f(Nil, fun) :: args.map(f(Nil, _))
    case ExprCaseOf(expr, cases) =>
      f(Nil, expr) :: cases.map{ case (c, vs, b) => f(vs, b) }
    case ExprLet(expr, binds) =>
      f(binds.map(_._1), expr) :: binds.map{ case (v, b) => f(Nil, b) }
    case _ => Nil
  }
  
  def allSubExprs: List[Expr] =
    this :: mapChildrenToList((_, b) => b.allSubExprs).flatten
  
  def resolveUnbound(global: Set[String], bound: Set[String] = Set()): Expr = this match {
    case ExprVar(v) if bound.contains(v) => ExprVar(v)
    case ExprVar(v) if global.contains(v) => ExprFun(v)
    case ExprVar(v) => ExprVar(v)
    case ExprCall(ExprVar(v), as) if !bound.contains(v) =>
      ExprCall(ExprFun(v), as.map(_.resolveUnbound(global, bound)))
    case _ => mapChildren((newvs, b) => b.resolveUnbound(global, bound ++ newvs))
  }
  
  def mergeAppsAndLambdas: Expr = this match {
    case ExprLambda(Nil, b) => b.mergeAppsAndLambdas
    case ExprCall(b, Nil) => b.mergeAppsAndLambdas
    case ExprLambda(vs, b) =>
       b.mergeAppsAndLambdas match {
         case ExprLambda(us, b1) => ExprLambda(vs ++ us, b1)
         case b1 => ExprLambda(vs, b1)
       }
    case ExprCall(b, as) =>
       b.mergeAppsAndLambdas match {
         case ExprCall(b1, as1) => ExprCall(b1, as1 ++ as)
         case b1 => ExprCall(b1, as)
       }
    case _ => mapChildren((_, b) => b.mergeAppsAndLambdas)
  }
  
  def freeVars: Set[String] = this match {
    case ExprVar(v) => Set(v)
    case _ => (Set[String]() /: mapChildrenToList((vs, b) => b.freeVars -- vs))(_ ++ _)
  }
  
  def bindUnbound: Expr = {
    val fv = freeVars
    if(fv.isEmpty) 
      this
    else
      ExprLambda(fv.toList, this)
  }
  
  def isConst: Boolean = this match {
    case ExprConstr(_) => true
    case ExprCall(ExprConstr(_), as) =>
      as.forall(_.isConst)
    case _ => false
  }
  
  def mkConst: Value = this match {
    case ExprConstr(c) => Ctr(c, Nil)
    case ExprCall(ExprConstr(c), as) =>
      Ctr(c, as.map(_.mkConst))
    case _ => throw new Exception("mkConst from a non-const")
  }
  
  def factorConstsImpl: (Expr, Set[(String, Expr)]) = {
    if(isConst)
      (ExprVar("$c " + this), Set("$c " + this -> this))
    else {
      val set = collection.mutable.Set[(String, Expr)]()
      val e = 
        mapChildren{(_, b) =>
          val (e, s) = b.factorConstsImpl
          set ++= s
          e
        }
      (e, set.toSet)
    }
  }
  
  def factorConsts: Expr = {
    val (e, ps) = factorConstsImpl
    val psl = ps.toList
    ExprCall(ExprLambda(psl.map(_._1), e), psl.map(_._2))
  }
  
  def loadInto(g: NamedNodes, table: Map[String, Int] = Map()): RenamedNode = this match {
    case ExprLambda(vs, b) =>
      // Top-level lambdas can be loaded into a graph
      b.loadInto(g, vs.zipWithIndex.toMap)
    case ExprCall(ExprFun(f), as) =>
      val n = g.newNode(f, as.size)
      assert(n.arity <= as.size)
      g.add(Let(), n :: as.map(_.loadInto(g, table)))
    case ExprCall(ExprConstr(c), as) =>
      g.add(Construct(c), as.map(_.loadInto(g, table)))
    case ExprCall(b, as) =>
      throw new Exception("An unknown function call cannot be loaded into the graph:\n" + this)
    case ExprConstr(c) =>
      g.add(Construct(c), Nil)
    case ExprFun(f) =>
      g(f)
    case ExprVar(v) =>
      val i = table.getOrElse(v, throw new Exception("Unbound variable: " + this))
      g.variable(i)
    case ExprUnused() =>
      g.unused
    case ExprCaseOf(expr, cases) =>
      val casenodes =
        cases.map{ case (c, vs, b) => 
          val vssize = vs.size
          val newtable = table.mapValues(_ + vssize) ++ vs.zipWithIndex
          ((c, vssize), b.loadInto(g, newtable)) }.sortBy(_._1)
      g.add(CaseOf(casenodes.map(_._1)), expr.loadInto(g, table) :: casenodes.map(_._2))
    case ExprLet(expr, bnds) =>
      val bssize = bnds.size
      val newtable = table.mapValues(_ + bssize) ++ bnds.map(_._1).zipWithIndex
      val body = expr.loadInto(g, newtable)
      val args = 
        (0 until body.arity).map(i => 
          if(i < bssize) bnds(i)._2.loadInto(g, table)
          else g.variable(i - bssize))
      g.add(Let(), body :: args.toList)
  }
  
  override def toString: String = this match {
    case ExprLambda(vars, body) => "\\" + vars.mkString(" ") + " -> " + body
    case ExprFun(name) => name
    case ExprVar(name) => name
    case ExprConstr(name) => name
    case ExprUnused() => "_" 
    case ExprCall(fun, args) => "(" + fun + ") " + args.map("(" + _ + ")").mkString(" ") 
    case ExprCaseOf(expr, cases) => 
      val cs = cases.map{ case (c, vs, b) => c + vs.mkString(" ", " ", " ") + "-> " + b + "; " }
      "case " + expr + " of { " + cs.mkString  + "}"
    case ExprLet(expr, binds) =>
      val bs = binds.map{ case (v, b) => v + " = " + b + "; " }
      "let " + bs.mkString + "in " + expr
  }
}

case class Program(
    defs: Map[String, List[Expr]] = Map.empty,
    propdefs: Map[String, Prop] = Map.empty,
    tests: List[Expr] = Nil,
    roots: List[String] = Nil,
    residualize: List[Expr] = Nil,
    assumptions: List[Prop] = Nil,
    prove: List[Prop] = Nil) {
  
  // TODO: other components
  override def toString: String =
    (for((n, bs) <- defs.toList; b <- bs) yield n + " = " + b + ";\n").mkString + 
    tests.map("test:" + _ + ";\n").mkString +
    "root: " + roots.mkString(", ") + ";\n" +
    residualize.map("residualize: " + _ + ";\n").mkString +
    assumptions.map("assume: " + _ + ";\n").mkString +
    prove.map("prove: " + _ + ";\n").mkString
    
  
  def ++(o: Program): Program =
    Program(
        defs ++ o.defs.map{ case (n,l) => (n, l ++ defs.getOrElse(n, Nil)) },
        propdefs ++ o.propdefs.map{ 
          case (n,l) if propdefs.contains(n) => throw new Exception("Redefinition of prop " + n)
          case (n,l) => (n,l) },
        tests ++ o.tests,
        roots ++ o.roots,
        residualize ++ o.residualize,
        assumptions ++ o.assumptions,
        prove ++ o.prove)
    
  // map all top-level expressions
  def mapExprs(f: Expr => Expr): Program = {
    Program(
      defs.mapValues(_.map(f)).view.force, 
      propdefs.mapValues(_.mapExprs(f)).view.force,
      tests.map(f),
      roots,
      residualize.map(f),
      assumptions.map(_.mapExprs(f)),
      prove.map(_.mapExprs(f)))
  }
  
  // all top-level expressions
  def allExprs: Iterable[Expr] =
      defs.values.flatten ++ propdefs.values.flatMap(_.allExprs) ++ tests ++ 
      residualize ++ assumptions.flatMap(_.allExprs) ++ prove.flatMap(_.allExprs)
  
  def allSubExprs: Iterable[Expr] = allExprs.flatMap(_.allSubExprs)
      
  def resolveUnbound: Program =
    mapExprs(_.resolveUnbound(defs.keySet))
    
  def mergeAppsAndLambdas: Program =
    mapExprs(_.mergeAppsAndLambdas)
    
  // if we have several definitions of a single function,
  // we should make them all have equal arity. 
  def splitBadLambdas: Program = {
    val newdefs = defs.mapValues{ ds =>
        val ar = (Int.MaxValue :: ds.map(_.arity)).min
        ds.map{
          case ExprLambda(vs, b) if vs.size > ar =>
            ExprLambda(vs.take(ar), ExprLambda(vs.drop(ar), b))
          case e => e
        }
      }.view.force
    Program(newdefs, propdefs, tests, roots, residualize, assumptions, prove)
  }
  
  // allows writing f = g instead of f x = g x in props
  def topLevelEtaExpand: Program = {
    def go(e: Expr): Expr = e match {
      case e@ExprCall(ExprFun(name), as) if defs.contains(name) =>
        // I think we should use the maximal possible arity here
        // because after the procedure the minimal arity may increase
        val ar = (0 :: defs(name).map(_.arity)).max
        if(ar <= as.size) e
        else {
          val args = (0 until (ar - as.size)).map("$eta_" + _).toList
          ExprLambda(args, ExprCall(ExprFun(name), as ++ args.map(ExprVar(_))))
        }
      case ExprFun(name) => go(ExprCall(ExprFun(name), Nil))
      case ExprLambda(vs, body) => ExprLambda(vs, go(body)).mergeAppsAndLambdas
      case _ => e
    }
    mapExprs(go)
  }
    
  def liftLambdas: Program = {
    val newdefs = collection.mutable.Map[String, List[Expr]]()
    var num = 1
    def newname() = {
      var nm = ""
      do {
        nm = "$closure_" + num 
        num += 1
      } while(defs.contains(nm))
      nm
    }
    
    def go(e: Expr): Expr = e match {
      case ExprLambda(vs, b) =>
        val newb = go(b)
        val name = newname()
        val free = (newb.freeVars -- vs).toList
        newdefs += name -> List(ExprLambda(free ++ vs, newb))
        ExprCall(ExprFun(name), free.map(ExprVar(_)))
      case _ => e.mapChildren((_,b) => go(b))
    }
    
    // TODO: This is wrong if tests contain top-level lambdas
    val newprog = mapExprs {
      case ExprLambda(vs, b) => ExprLambda(vs, go(b))
      case b => go(b)
    }
    
    newprog ++ Program(defs = newdefs.toMap)
  }
    
  // TODO: Sometimes it may be proved that certain constructors are equal which is bad
  def defunctionalize: Program = {
    var needApp = false
    val minargs = collection.mutable.Map[String, Int]()
    
    // Some functions may be undefined, so we should guess their arities
    val arities =
      allSubExprs.collect{ case ExprCall(ExprFun(f), as) => (f, as.size) }
        .groupBy(_._1).mapValues(_.map(_._2).max) ++ 
          defs.collect{ case (f, l@(_::_)) => (f, l.map(_.arity).min) }
    
    def makeapps(f: Expr, as: List[Expr]) =
        (f /: as)((l,r) => ExprCall(ExprFun("@"), List(l, r)))
      
    def go(e: Expr): Expr = e match {
      case ExprFun(f) =>
        go(ExprCall(ExprFun(f), Nil))
      case ExprCall(ExprConstr(c), as) => 
        ExprCall(ExprConstr(c), as.map(go(_)))
      case ExprCall(ExprFun(f), as) =>
        val arity = arities(f)
        val newas = as.map(go(_))
        if(arity == newas.size)
          ExprCall(ExprFun(f), newas)
        else if(arity < newas.size) {
          needApp = true
          val newf = ExprCall(ExprFun(f), newas.take(arity))
          makeapps(newf, newas.drop(arity))
        } else {
          minargs(f) = minargs.getOrElse(f, newas.size) min newas.size
          ExprCall(ExprConstr("#Ptr_" + f + "_" + newas.size), newas)
        }
      case ExprCall(f, as) =>
        needApp = true
        val newf = go(f)
        val newas = as.map(go(_))
        makeapps(newf, newas)
      case _ => e.mapChildren((_,b) => go(b))
    }
    
    val newprog = mapExprs(go)
    val appcases = 
      (for((f,m) <- minargs.toList; ar = arities(f); k <- m until ar) yield
        ("#Ptr_" + f + "_" + k, (0 until k).map("$" + _).toList, 
            if(k < ar - 1)
              ExprCall(ExprConstr("#Ptr_" + f + "_" + (k+1)), 
                  (0 until k).map(i => ExprVar("$" + i)).toList ++ List(ExprVar("$arg"))) : Expr
            else
              ExprCall(ExprFun(f),
                  (0 until k).map(i => ExprVar("$" + i)).toList ++ List(ExprVar("$arg"))))
      ) ++ List(("#Ptr_", List("$ext"), 
            ExprCall(ExprFun("@ext"), List(ExprVar("$ext"), ExprVar("$arg")))))
            
    if(needApp)
      newprog ++ 
        Program(defs = 
          Map("@" -> List(ExprLambda(List("$fun", "$arg"), 
                            ExprCaseOf(ExprVar("$fun"), appcases)))))
    else
      newprog
  }
  
  // Factor out consts from the tests.
  def splitTests: Program = {
    val newtests = tests.map(_.factorConsts)
    Program(defs, propdefs, newtests, roots, residualize, assumptions, prove)
  }
  
  def simplify: Program = {
    val p1 = this.resolveUnbound.mergeAppsAndLambdas.topLevelEtaExpand
    //println("Resolved and merged:\n" + p1)
    val p2 = p1.splitBadLambdas
    //println("Split bad:\n" + p2)
    val p3 = p2.liftLambdas
    //println("Lifted:\n" + p3)
    val p4 = p3.mergeAppsAndLambdas.defunctionalize
    //println("Defunctionalized:\n" + p4)
    p4.splitTests
  }
    
  // Removes unreferenced functions from defs
  def removeUnreferenced: Program = {
    val rexpr =
      propdefs.values.flatMap(_.allExprs) ++ tests ++ residualize ++
      assumptions.flatMap(_.allExprs) ++ prove.flatMap(_.allExprs)
    val funs = collection.mutable.Set[String]()
    val stack = collection.mutable.Stack[String](roots:_*)
    for(ExprFun(f) <- rexpr.flatMap(_.allSubExprs))
      stack.push(f)
    while(stack.nonEmpty) {
      val f = stack.pop()
      if(!funs(f)) {
        funs += f
        for(es <- defs.get(f); e <- es; ExprFun(g) <- e.allSubExprs)
          stack.push(g)
      }
    }
      
    Program(defs.filterKeys(funs(_)), propdefs, tests, roots, residualize, assumptions, prove)
  }
  
  // Prepend "q." to every function name
  def qualify(q: String): Program = {
    def qual(l: Any, e: Expr): Expr = e match {
      case ExprFun(f) => ExprFun(q + "." + f)
      case _ => e.mapChildren(qual)
    }
    val p = mapExprs(qual(null, _))
    Program(p.defs.map(p => q + "." + p._1 -> p._2), 
      p.propdefs, p.tests, p.roots.map(q + "." + _), p.residualize, p.assumptions, p.prove)
  }
    
  def loadInto(g: NamedNodes) {
    for((f, ds) <- defs if ds.nonEmpty) {
      val ars = ds.map(_.arity)
      val ar = ars.head
      assert(ars.forall(_ == ar))
      val node = g.newNode(f, ar)
      for(d <- ds)
        try {
            g.glue(List(node, d.loadInto(g)))
        } catch {
          case e: Throwable => 
            System.err.println("Error while loading " + f)
            System.err.println(f + " = " + d)
            throw e
        }
    }
    
    for(a <- assumptions)
      a.loadInto(g)
  }
  
  def loadTestsInto(g: NamedNodes with HyperTester) {
    for(t <- tests) t match {
      case ExprLambda(vs, _) if vs.nonEmpty =>
        throw new Exception("Top-level lambdas in tests are not allowed: " + t)
      case _ if t.freeVars.nonEmpty =>
        throw new Exception("Free variables in tests are not allowed: " + t)
      case ExprCall(e, as) if as.forall(_.isConst) =>
        g.runNode(e.loadInto(g), as.map(_.mkConst))
      case _ =>
        g.runNode(t.loadInto(g), Nil)
    }
  }
}

class ProgramParser(path: String) extends RegexParsers {
  override val whiteSpace = """(\s|--.*\n)+""".r
  def fname = not("of\\b".r) ~> "[a-z$][a-zA-Z0-9$.@_]*".r
  def cname = "[A-Z#][a-zA-Z0-9$.@_]*".r
  def varname = fname | "_"
  
  def apply(g: NamedNodes, s: String): Program = {
    val p = parseProg(s).simplify
    p.loadInto(g)
    p
  }
  
  def parseProg(s: String): Program = {
    val parsed = parseAll(prog, s)
    if(!parsed.successful) {
      System.err.println("Syntax error at " + parsed.next.pos + ": " + parsed.next.first)
      throw new Exception("Unsuccessful parse")
    }
    parsed.get
  }
  
  def prog: Parser[Program] = 
    (repsep(decl, ";") <~ opt(";")).map(l => (Program() /: l)(_ ++ _))
  
  def decl: Parser[Program] =
    definition | eqdecl | propdecl | testdecl | rootdecl | residdecl | assumedecl | provedecl |
    decldecl | include
    
  def file = 
    ("\"[^\"]+\"" ^^ (s => s.substring(1,-1)) | """[^;\s]+""".r) ~ opt("as" ~> fname) ^^ { 
        case s~None => ProgramParser.parseFile(path + "/" + s).resolveUnbound
        case s~Some(q) => ProgramParser.parseFile(path + "/" + s).resolveUnbound.qualify(q)
      }
  
  def include: Parser[Program] =
    ("include:" ~> file) |
    ("import:" ~> file ~ ("(" ~> repsep(fname, ",") <~ ")")) ^^ 
      { case p~ns => Program(Program(defs = p.defs, roots = ns).removeUnreferenced.defs) }
    
  def definition: Parser[Program] =
    (fname ~ rep(varname) <~ "=") ~! expr ^^
      { case f~as~e => Program(defs = Map(f -> List(ExprLambda(as, e)))) }
    
  def eqdecl: Parser[Program] = 
    (expr <~ "=") ~! expr ^^ { case e1~e2 => Program(assumptions = List(PropEq(e1, e2))) }
  
  def propdecl: Parser[Program] = 
    ("prop" ~> fname <~ ":") ~! prop ^^ 
      { case n~p => Program(propdefs = Map(n -> p)) }
  
  def assumedecl: Parser[Program] = 
    ("assume:" ~> prop) ^^ (p => Program(assumptions = List(p)))
  
  def provedecl: Parser[Program] = 
    ("prove:" ~> prop) ^^ (p => Program(prove = List(p)))
    
  def rootdecl: Parser[Program] = 
    ("root:" ~> repsep(fname, ",")) ^^ (fs => Program(roots = fs))
  
  def testdecl: Parser[Program] = 
    ("test:" ~> repsep(expr, ",")) ^^ (es => Program(tests = es))
    
  def residdecl: Parser[Program] = 
    ("residualize:" ~> repsep(expr, ",")) ^^ (es => Program(residualize = es))
  
  def decldecl: Parser[Program] =
    ("declare:" ~> repsep(fname, ",")) ^^ (fs => Program(defs = fs.map(f => (f, Nil)).toMap))
    
  def prop: Parser[Prop] =
    (expr <~ "=") ~ expr ^^ { case e1~e2 => PropEq(e1, e2) } |
    (expr <~ "~") ~ expr ^^ { case e1~e2 => PropEqModuloRen(e1, e2) } |
    (expr <~ "^") ~ cname ^^ { case e~c => PropReturnsConstr(e, c) } |
    fname ^^ (n => PropNamed(n))
  
  def expr: Parser[Expr] =
    caseof |
    let |
    lambda |
    rep1(argexpr) ^^ { l => ExprCall(l.head, l.tail) }
    
  def argexpr: Parser[Expr] =
    fname ^^ (n => ExprVar(n)) |
    "_" ^^ (n => ExprUnused()) |
    cname ^^ (n => ExprConstr(n)) |
    "(" ~> expr <~ ")"
  
  def caseof: Parser[ExprCaseOf] =
    ("case" ~> expr <~ "of") ~! ("{" ~> repsep(onecase, ";") <~ opt(";") <~ "}") ^^
      { case e~cases => ExprCaseOf(e, cases) }
    
  def onecase: Parser[(String, List[String], Expr)] =
    cname ~ rep(varname) ~ "->" ~ expr ^^
      { case n~l~"->"~e => (n, l, e) }
  
  def let: Parser[ExprLet] =
    ("let" ~> repsep(fname ~ expr, ";") <~ "in") ~ expr ^^
      { case lst~e => ExprLet(e, lst.map{ case n~e => (n,e) }) }
  
  def lambda: Parser[ExprLambda] =
    ("\\" ~> rep(varname) <~ "->") ~ expr ^^
      { case vs~e => ExprLambda(vs, e) }
}

object ProgramParser extends ProgramParser("") {
  def parseFile(path: String): Program = {
    val src = io.Source.fromFile(path)
    val srctext = src.mkString
    src.close()
    (new ProgramParser((new File(path)).getParent())).parseProg(srctext)
  }
}
