package graphsc.app

import graphsc._


object Reformat {

  trait MyType {
    def unify(other: MyType): MyType = (this, other) match {
      case (EmptyType(), t) => t
      case (t, EmptyType()) => t
      case (a, b) if a == b => a
      case (SumType(s1), SumType(s2)) => SumType(s1 | s2)
      case (FunType(a1, b1), FunType(a2, b2)) => FunType(a1 unify a2, b1 unify b2)
      case (t1:MutType, t2) =>
        assert(!t2.containsStrictly(t1))
        if(t2.contains(t1))
          t2
        else
          t1.ref match {
            case None =>
              t1.ref = Some(t2)
              t1
            case Some(t) =>
              t1.ref = Some(t unify t2)
              t1
          }
      case (t1, t2:MutType) => t2 unify t1
      case _ => AnyType()
    }
    
    def containsStrictly(t: MyType): Boolean = this match {
      case FunType(t1, t2) => t1.contains(t) || t2.contains(t)
      case t1:MutType => t1.ref.map(_.containsStrictly(t)).getOrElse(false)
      case _ => false
    }
    
    def contains(t: MyType): Boolean = this match {
      case _ if this == t => true
      case FunType(t1, t2) => t1.contains(t) || t2.contains(t)
      case t1:MutType => t1.ref.map(_.contains(t)).getOrElse(false)
      case _ => false
    }
    
    def deref: MyType = this match {
      case t:MutType => t.ref.map(_.deref).getOrElse(t)
      case FunType(f, t) => FunType(f.deref, t.deref)
      case _ => this
    }
    
    def |(other: MyType): MyType = (this, other) match {
      case (EmptyType(), t) => t
      case (t, EmptyType()) => t
      case (a, b) if a == b => a
      case (SumType(s1), SumType(s2)) => SumType(s1 | s2)
      case (FunType(a1, b1), FunType(a2, b2)) => FunType(a1 & a2, b1 | b2)
      case (t1:MutType, t2) =>
        t1.ref match {
          case None => t2
          case Some(t) => t | t2
        }
      case (t1, t2:MutType) => t2 | t1
      case _ => AnyType()
    }
    
    def &(other: MyType): MyType = (this, other) match {
      case (AnyType(), t) => t
      case (t, AnyType()) => t
      case (a, b) if a == b => a
      case (SumType(s1), SumType(s2)) => SumType(s1 & s2)
      case (FunType(a1, b1), FunType(a2, b2)) => FunType(a1 | a2, b1 & b2)
      case (t1:MutType, t2) =>
        t1.ref match {
          case None => t2
          case Some(t) => t & t2
        }
      case (t1, t2:MutType) => t2 & t1
      case _ => EmptyType()
    }
  }
  
  case class SumType(cs: Set[String]) extends MyType
  case class FunType(from: MyType, to: MyType) extends MyType
  case class EmptyType() extends MyType
  case class AnyType() extends MyType
  
  class MutType(var ref: Option[MyType] = None) extends MyType {
    override def toString = "(" + ref.map(_.toString).getOrElse(super.toString) + ")"
  }
  
  def mergeSets[T](sets: List[Set[T]]): List[Set[T]] = sets match {
    case Nil => Nil
    case (h::t) =>
      val tail = mergeSets(t)
      var cur = h
      var newtail: List[Set[T]] = Nil
      for(s <- tail) {
        if((cur & s).nonEmpty)
          cur = cur | s
        else
          newtail = s :: newtail
      }
      cur :: newtail
  }
  
  // This function is very slow, may hang or even give incorrect result if it cannot infer types,
  // so don't use it unless you know what you are doing
  def guessTypes(prog: Program): 
        (Map[String, Set[String]], Map[String, List[MyType]], Map[ExprCaseOf, MyType]) = {
    val types1 =
      mergeSets(prog.allSubExprs.collect{
        case ExprCaseOf(_, cs) => cs.map(_._1).toSet
      }.toList)
      
    val cons2type1 =
      types1.map(s => s.toList.map((_, s))).flatten.toMap
    
    val othertype =
      prog.allSubExprs.collect{
        case ExprConstr(c) if !cons2type1.contains(c) => c
      }.toSet
    
    val cons2type = 
      if(othertype.isEmpty) cons2type1 
      else cons2type1 ++ othertype.map((_, othertype))
      
    val argtypes = collection.mutable.Map[(String, Int), List[MyType]]()
    val caseoftypes = collection.mutable.Map[ExprCaseOf, List[MyType]]()
    
    for((s,bs) <- prog.defs)
      go(ExprFun(s))
      
    for(p <- prog.prove; e <- p.allExprs)
      go(e)
      
    def go(e: Expr, hist: Map[String, MyType] = Map(), 
                    varts: Map[String, MyType] = Map()): MyType =  e match {
      case ExprFun(n) if hist.contains(n) => hist(n)
      case ExprFun(n) =>
        var t: MyType = new MutType()
        for(b <- prog.defs(n)) {
          t = t unify go(b, hist + (n -> t), varts)
        }
        t
      case ExprVar(v) => varts.getOrElse(v, new MutType())
      case ExprUnused() => new MutType()
      case ExprLambda(vs, body) =>
        val lst = vs.map(v => (v, new MutType()))
        val bt = go(body, hist, varts ++ lst)
        (lst.map(_._2) :\ bt)(FunType(_, _))
      case ExprLet(e, bs) =>
        go(e, hist, varts ++ bs.map(p => (p._1, go(p._2, hist, varts))))
      case ExprConstr(c) => go(ExprCall(ExprConstr(c), Nil), hist, varts)
      case ExprCall(ExprConstr(c), as) =>
        for((a,i) <- as.zipWithIndex)
          argtypes.withDefaultValue(Nil)((c,i)) ::= go(a, hist, varts)
        SumType(cons2type(c))
      case ExprCall(f, Nil) => go(f, hist, varts)
      case ExprCall(f, List(e)) =>
        val res = new MutType()
        go(f, hist, varts) unify FunType(go(e, hist, varts), res)
        res
      case ExprCall(f, a :: as) => go(ExprCall(ExprCall(f, List(a)), as), hist, varts)
      case expr@ExprCaseOf(e, cs) =>
        go(e, hist, varts) unify SumType(cons2type(cs(0)._1))
        val ts =
          for((c, vs, b) <- cs) yield {
            val lst = vs.map(v => (v, new MutType()))
            for(((_, t), i) <- lst.zipWithIndex)
              argtypes.withDefaultValue(Nil)((c,i)) ::= t
            go(b, hist, varts ++ lst)
          }
        val rtype = ts.reduce(_ unify _)
        caseoftypes.withDefaultValue(Nil)(expr) ::= rtype
        rtype
    }
      
    val argtypesred = //argtypes.mapValues(_.map(_.deref).toSet) 
      //argtypes.map(p => (p._1, p._2.reduce(_ | _)))
      argtypes.mapValues(_.map(_.deref)).map(p => (p._1, p._2.reduce(_ unify _).deref))
    
    val caseoftypesred = 
      caseoftypes.mapValues(_.map(_.deref)).map(p => (p._1, p._2.reduce(_ unify _).deref))
      
    (cons2type, 
     argtypesred.groupBy(_._1._1).mapValues(x => x.toList.sortBy(_._1._2).map(_._2))
       .withDefaultValue(Nil), 
     caseoftypesred.toMap)
  }
  
  def guessTypevars(cons2type: Map[String, Set[String]], argtypes: Map[String, List[MyType]]):
        Map[Set[String], Set[String]] = {
    val types = cons2type.values.toSet
    val state = collection.mutable.Map[Set[String], Set[String]]().withDefaultValue(Set())
    
    def typeVars(t: MyType, pref: String): Set[String] = t match {
      case SumType(s) if types(s) => state(s)
      case FunType(f, t) => typeVars(f, pref + "f_") ++ typeVars(t, pref + "t_")
      case _ => Set(pref + "x")
    } 
    
    var changed = true
    while(changed) {
      changed = false
      for(t <- types) {
        val newvars = 
          (for(c <- t.toList; (at,j) <- argtypes(c).zipWithIndex) yield 
              typeVars(at, "a_" + typeName(t) + "_" + c + "_" + j + "_")
          ).flatten.toSet
        if(state(t) != newvars) {
          changed = true
          state(t) = newvars
        }
      }
    }
    state.toMap.withDefaultValue(Set())
  }
  
  def typeName(t: Set[String]): String = {
    if(t == Set("S", "Z")) "Nat"
    else if(t == Set("C", "N")) "Lst"
    else if(t == Set("T", "F")) "Boolean"
    else if(t == Set("Cons", "Nil")) "List"
    else "T_" + t.mkString("_")
  }
  
  def getSumTypes(t: MyType): List[Set[String]] = t match {
    case SumType(s) => List(s)
    case FunType(f, t) => getSumTypes(f) ++ getSumTypes(t)
    case _ => Nil
  }
  
  def mapSumTypes(t: MyType, cons2type: String => Set[String], 
                  myany: MyType => MyType = x => x): MyType = t match {
    case SumType(s) => SumType(cons2type(s.head))
    case FunType(f, t) => 
      FunType(mapSumTypes(f, cons2type, myany), mapSumTypes(t, cons2type, myany))
    case _ => myany(t)  
  }
  
  def mergeTypes(force_partial: Boolean, 
      t: (Map[String, Set[String]], Map[String, List[MyType]], Map[ExprCaseOf, MyType]) ):
        (Map[String, Set[String]], Map[String, List[MyType]], Map[ExprCaseOf, MyType], 
            Map[Set[String], String]) = {
    if(force_partial) {
      val types = mergeSets(
          t._1.values.toList ++ 
          t._2.values.flatten.flatMap(getSumTypes(_)) ++ t._3.values.flatMap(getSumTypes(_)))
      
      val myany = Set("My_S", "My_Z", "My_Bot")
          
      val typesbot = 
        types.zipWithIndex.map(p => (p._1 + ("Bottom_" + p._2), "Bottom_" + p._2)).toMap + 
        (myany -> "My_Bot")
          
      val cons2type =
        typesbot.keys.map(s => s.toList.map((_, s))).flatten.toMap
      
      (cons2type, 
       (t._2.mapValues(_.map(mapSumTypes(_, cons2type, _ => SumType(myany)))) ++
         Set("My_Z" -> Nil, "My_Bot" -> Nil, "My_S" -> List(SumType(myany))))
           .withDefaultValue(Nil),
       t._3.mapValues(mapSumTypes(_, cons2type, _ => SumType(myany))),
       typesbot)
    } else
      (t._1, t._2, t._3, null)
  }
  
  trait ProverName
  case object Hosc extends ProverName
  case object Hipspec extends ProverName
  case object Zeno extends ProverName
  case object SpeedBenchmark extends ProverName
  
  def apply(prog: Program, frmt: String) {
    val (prover, force_partial) = frmt match {
      case "hosc" => (Hosc, false)
      case "hipspec-total" => (Hipspec, false)
      case "hipspec-partial" => (Hipspec, true)
      case "zeno-total" => (Zeno, false)
      case "zeno-partial" => (Zeno, true)
      case "speed-benchmark" => (SpeedBenchmark, false)
    }
    
    val (cons2type, argtypes, caseoftypes, bottoms) = 
      mergeTypes(force_partial, guessTypes(prog))
    val types = cons2type.values.toSet
    
//    for((c,ts) <- argtypes) {
//      println(c + ":")
//      for(t <- ts) {
//        println("  " + t)
//      }
//    }
    
    if(prover == SpeedBenchmark) {
      println("{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DefaultSignatures #-}")
      println("module Test where\n")
      println("import qualified Prelude")
      println("import Prelude (Eq, Ord, Show, ($), (==), (-), seq)")
      println("import Control.Applicative ((<$>), (<*>))")
      println("import Data.Typeable (Typeable)")
      println("import Test.QuickCheck")
      println("import Control.DeepSeq")
      println("import GHC.Generics (Generic)")
      println("import Data.Serialize (Serialize)")
    } else if(prover == Hipspec) {
      println("{-# LANGUAGE DeriveDataTypeable #-}")
      println("module Test where\n")
      println("import qualified Prelude")
      println("import Prelude (Eq, Ord, Show, Int, ($), (==), (-), (/))")
      println("import Control.Applicative ((<$>), (<*>))")
      println("import HipSpec")
    } else if(prover == Zeno) {
      println("module Test where\n")
      println("import qualified Prelude")
      println("import Prelude (Eq, Ord, Show, Int, ($), (==), (-), (/))")
      println("import Control.Applicative ((<$>), (<*>))")
      println("import Zeno")
    }
    
    println()
    
    {
      val tvars = guessTypevars(cons2type, argtypes)
      
      def typeToStr(t: MyType, pref: String): String = t match {
        case SumType(s) if types(s) => 
          "(" + typeName(s) + tvars(s).mkString(" ", " ", "") + ")"
        case FunType(f, t) => 
          "(" + typeToStr(f, pref + "f_") + " -> " + typeToStr(f, pref + "t_") + ")"
        case _ => pref + "x"
      }
      
      for(t <- types) {
        val conslist =
          for(c <- t.toList) yield {
            val args =
              for((at,j) <- argtypes(c).zipWithIndex) yield 
                typeToStr(at, "a_" + typeName(t) + "_" + c + "_" + j + "_")
            c + args.mkString(" ", " ", "")
          }
        print("data " + typeName(t) + " " + tvars(t).mkString(" ")) 
        print(conslist.mkString(" = ", " | ", ""))
        
        if(prover == Hipspec || prover == SpeedBenchmark) {
          if(prover == SpeedBenchmark)
            println(" deriving (Eq, Ord, Show, Typeable, Generic)")
          else
            println(" deriving (Eq, Ord, Show, Typeable)")
          println()
          
          println("instance " + tvars(t).map("Show " + _).mkString("(",",",") => ") +
              "CoArbitrary (" + typeName(t) + " " + tvars(t).mkString(" ") + ") where")
          println("  coarbitrary = coarbitraryShow")
          println()
          
          if(prover == Hipspec) {
            println("instance " + tvars(t).map("Partial " + _).mkString("(",",",") => ") +
                "Partial (" + typeName(t) + " " + tvars(t).mkString(" ") + ") where")
            for(c <- t) {
              if(argtypes(c).isEmpty)
                println("  unlifted " + c + " = Prelude.return " + c)
              else {
                val nums = argtypes(c).toList.indices.toList.map("x" + _)
                println("  unlifted (" + c + nums.mkString(" ", " ", "") + ") = " +
                    c + " <$> " +
                    nums.map("(lifted " + _ + ")").mkString(" <*> "))
              }
            }
            println()
          }
          
          if(prover == SpeedBenchmark) {
            println("instance " + tvars(t).map("Serialize " + _).mkString("(",",",") => ") +
                "Serialize (" + typeName(t) + " " + tvars(t).mkString(" ") + ")\n")
            
            println("instance " + tvars(t).map("NFData " + _).mkString("(",",",") => ") +
                "NFData (" + typeName(t) + " " + tvars(t).mkString(" ") + ") where")
            for(c <- t) {
              if(argtypes(c).isEmpty)
                println("  rnf " + c + " = ()")
              else {
                val nums = argtypes(c).toList.indices.toList.map("x" + _)
                println("  rnf (" + c + nums.mkString(" ", " ", "") + ") = " +
                    nums.map("(rnf " + _ + ")").mkString(" `seq` "))
              }
            }
            println()
          }
          
          println("instance " + tvars(t).map("Arbitrary " + _).mkString("(",",",") => ") +
              "Arbitrary (" + typeName(t) + " " + tvars(t).mkString(" ") + ") where")
          println("  arbitrary = sized $ \\s -> do")
          if(t.exists(argtypes(_).isEmpty)) {
            println("    if s == 0 then")
            println("      elements " + 
                t.filter(argtypes(_).isEmpty).mkString("[",",","]"))
            println("    else do")
          }
          
          if(prover == SpeedBenchmark) {
            val zero_ar = t.filter(argtypes(_).isEmpty)
            val nonzero_ar = t.filter(argtypes(_).nonEmpty) 
            
            if(zero_ar.isEmpty) println("      c_deeper (s - 1)")
            else if(nonzero_ar.isEmpty) println("      c_finish")
            else println("      frequency [(s - 1, c_deeper (s - 1)), (1, c_finish)]")
            
            println("    where")
            println("     c_finish = elements " + zero_ar.mkString("[", ", ", "]"))
            println("     c_deeper s = oneof " + 
                nonzero_ar.map(c => c + " <$> " + 
                    argtypes(c).map(_ => "resize s arbitrary").mkString(" <*> "))
                  .mkString("[", ", ", "]"))
          } else {
            println("      x <- choose (0 :: Int, " + (t.size - 1) + ")")
            println("      case x of")
            for((c,i) <- t.zipWithIndex) {
              if(argtypes(c).isEmpty)
                println("        " + i + " -> Prelude.return " + c)
              else
                println("        " + i + " -> " + c + " <$> " + 
                    argtypes(c).map(_ => "resize (s `Prelude.div` 2) arbitrary").mkString(" <*> "))
            }
          }
          
          println()
        }
        else if(prover == Hosc)
          println(";\n")
        else
          println()
      }
    }
    
    println()
    
    var bndnum = 0;
    
    def renVar(v: String): (String, String) = {
      bndnum += 1
      (v, v + "_" + bndnum)
    }
    
    def renameBoundVars(e: Expr, map: Map[String, String] = Map()): Expr = e match {
      case _ if prover != Hosc => e
      case ExprVar(v) => ExprVar(map.getOrElse(v, v))
      case ExprLambda(vs, body) =>
        val m = vs.map(renVar).toMap
        ExprLambda(vs.map(m(_)), renameBoundVars(body, map ++ m))
      case ExprLet(e, bs) =>
        val m = bs.map(p => renVar(p._1)).toMap
        ExprLet(renameBoundVars(e, map ++ m), bs.map(p => (m(p._1), renameBoundVars(p._2, map))))
      case ExprCaseOf(e, cs) =>
        val m = cs.flatMap(_._2).map(renVar).toMap
        ExprCaseOf(renameBoundVars(e, map), 
            cs.map(t => (t._1, t._2.map(m), renameBoundVars(t._3, map ++ m))))
      case _ => e.mapChildren((_, e) => renameBoundVars(e, map))
    }
    
    if(prover == Hosc) {
      val List(PropEq(e1, e2)) = prog.prove.map(_.removeLambdas)
      println(renameBoundVars(e1) + " -- left-hand-side")
      println(renameBoundVars(e2) + " -- right-hand-side")
      
      println("where")
    }
    
    def mkBot(t: MyType): Expr = t match {
      case SumType(s) => ExprConstr(bottoms(s))
      case FunType(f, t) => ExprLambda(List("_"), mkBot(t))
      case _ => ExprConstr("My_Bot")
    }
    
    def adjustCaseofs(e: Expr): Expr = e match {
      case _ if bottoms == null => e
      case expr: ExprCaseOf =>
        val bot = mkBot(caseoftypes(expr))
        val ExprCaseOf(e, cs) = expr.mapChildren((_, e) => adjustCaseofs(e))
        ExprCaseOf(e, cs ++ 
            (cons2type(cs(0)._1) -- cs.map(_._1)).map(c => (c, argtypes(c).map(_ => "_"), bot)))
      case _ => e.mapChildren((_, e) => adjustCaseofs(e))
    }
    
    println("-- function definitions\n")
    
    for((name,bs) <- prog.defs; body <- bs) {
      println("-- function " + name)
      println(name + " = " + adjustCaseofs(renameBoundVars(body)) + ";")
    }
    
    if(prover == SpeedBenchmark) {
      for((body,i) <- prog.residualize.zipWithIndex) {
        println("-- function residual_" + i)
        println("residual_" + i + " = " + adjustCaseofs(renameBoundVars(body.bindUnbound)) + ";")
      }
    }
    
    println()
    
    if(prover == Hipspec || prover == Zeno) {
      var propnum = 0
      for(PropEq(e1, e2) <- prog.prove.map(_.removeLambdas)) {
        prover match {
          case Hipspec =>
            println("prop_" + propnum + " " + (e1.freeVars ++ e2.freeVars).mkString(" ") + " = " +
                "(" + adjustCaseofs(e1) + ") =:= (" + adjustCaseofs(e2) + ")" )
          case Zeno =>
            println("prop_" + propnum + " " + (e1.freeVars ++ e2.freeVars).mkString(" ") + " = " +
                "prove((" + adjustCaseofs(e1) + ") :=: (" + adjustCaseofs(e2) + "))" )
        }
        propnum += 1
      }
    }
    
  }
}