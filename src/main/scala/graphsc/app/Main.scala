package graphsc.app

import graphsc._
import interpretation._
import transformation._
import residualization._
import org.rogach.scallop._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("Equivalence prover based on hypergraph supercompilation, version 0.1")
  banner("Usage: EqProver [OPTIONS] file")

  val total = opt[Boolean](descr = "Assume that we work in total setting")
  
  val prove = opt[Boolean](descr = 
    "Prove the propositions specified in the input file and then exit")
  
  val resid = opt[Boolean](descr = "Residualize the function specified in the input file")
  val residAutoTest = opt[Int](noshort = true, descr = 
    "Run <arg> smallest automatically generated tests for each node on the defining boundary")
  val residAutoTestOnly = opt[Boolean](noshort = true, descr =
    "Don't use user-specified tests for residualization")
    
  val only = opt[Boolean](noshort = true, descr = "Don't load unreferenced functions into graph")
  
  val arity = opt[Int](default = Some(10), descr = "Maximal arity of nodes")
  val depth = opt[Int](default = Some(10), descr = "Depth limit")
  val codepth = opt[Int](default = Some(10), descr = "Codepth limit")
  val gen = opt[Boolean](noshort = true, descr = "Enable unordered generalization")
  val noiso = opt[Boolean](noshort = true, descr = "Disable merging by isomorphism")
  val generations = opt[Int](default = Some(1000), descr = "Maximal number of generations")
  val driveRecommended = opt[Int](noshort = true, default = Some(0), 
      descr = "Drive 2*<arg> recommended (by eqprover) nodes")
  val weakMerging = opt[Boolean](noshort = true, descr = "Disable merging up to renaming")
  val noLetToId = opt[Boolean](noshort = true, descr = "Disable destructive let to id conversion")
  val noLetReduce = opt[Boolean](noshort = true, descr = "Disable destructive let-var reduction")
  val noAutoReduce = opt[Boolean](noshort = true, descr = "Disable destructive reduction")
  val genPair = opt[Boolean](noshort = true, 
      descr = "Traditional pairwise generalization")
  val supercompile = opt[Boolean](name = "super", 
      descr = "Traditional multiresult supercompilation")
  val mergeUseless = opt[Boolean](noshort = true, 
      descr = "Merge nodes by iso even if that won't lead to more node merging")
  val onlyRequested = opt[Boolean](noshort = true, 
      descr = "Merge by iso only nodes specified in props")
  val drive2 = opt[Boolean](noshort = true, descr = "Use the second set of driving rules")
  val nobuf = opt[Boolean](noshort = true, 
      descr = "Disable bufferization of new edges, may lead to unstable behaviour")
  
  val gui = opt[Boolean](noshort = true, descr = "Launch GUI")
  val dumpDot = opt[Boolean](noshort = true, descr = "Dump the graph to stdout")
  val dumpDot2 = opt[Boolean](noshort = true, descr = "Dump the graph to stdout (lightweight)")
  val dumpCode = opt[Boolean](noshort = true, 
    descr = "Dump the graph to stdout in a form of a program")
  val dumpGenCode = opt[String](noshort = true, 
    descr = "Dump code for each generation to files <arg>-i")
  val verbose = opt[Boolean](descr = "Be more verbose")
  val log = opt[Boolean](descr = "Log transformations to stdout")
  val stat = opt[Boolean](descr = "Print some statistics like number of nodes at the end")
  val reformat = opt[String](noshort = true, 
      descr = "Transform the test to the specified format (hosc, hipspec, hipspec-total)")
  
  val file = trailArg[String](required = true)
  val output = opt[String](descr = "Output to a file instead of stdout")
  
  val nopretty = opt[Boolean](noshort = true, 
      descr="Disable transforming nodes to readable programs on the fly. " +
          "Makes everything a bit faster.")
  val test = opt[Boolean](noshort = true, descr = 
    "Enable testing on the fly using the tests specified in the input file")
  val testLog = opt[Boolean](noshort = true, descr = 
    "Enable tester logging")
  val integrityCheck = opt[Boolean](noshort = true, hidden = true)
  
  val cheat = opt[Boolean](noshort = true,
      descr = "Tweak the supercompiler so that it can solve the nrev example")
}

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

class MainHypergraphImplementation(conf: Conf) extends TheHypergraph
        with NamedNodes
        with GoalChecker
        with Transformations
        with BiTransformManager 
        with DepthTracker 
        with HyperTester
        with SelfLetAdder
        with AutoTransformer
        with Triggers 
        with Prettifier
        with HyperLogger
        with Visualizer {
  
  override val total = conf.total()
  override val integrityCheckEnabled = conf.integrityCheck()
  override val onTheFlyTesting = conf.test()
  override val prettifyingEnabled = !conf.nopretty()
  override val weakMerging = conf.weakMerging()
  override val autoLetToId = !conf.noLetToId()
  override val autoLetReduce = !conf.noLetReduce()
  override val autoReduce = !conf.noAutoReduce()
  override val enableVisualizer = conf.gui()
  
  override def filterUpdatedPairs(pairs: List[(Hyperedge, Hyperedge)]): 
      List[(Hyperedge, Hyperedge)] =
    pairs.filter { 
      case (h1,h2) =>
        nodesOf(h1,h2).map(depths(_)).max <= conf.depth() &&
        nodesOf(h1,h2).map(codepths(_)).max <= conf.codepth()
    }
  
  var enableLoggingVar = conf.log()
  override def enableLogging = enableLoggingVar
  override val hyperTesterLogging = conf.testLog()
  
  
  var residualizing = false
  override def limitFromMinCost(c: Double): Double =
    if(residualizing) Double.MaxValue
    else super.limitFromMinCost(c)
    
  override def hyperedgeCost(h: Hyperedge): Double = 
    super.hyperedgeCost(h) * (1 + (depth(h.source) + codepth(h.source))*0.2)
}

/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

object MainApp {
  
  def main(args: Array[String]) = mainBool(args) match {
    case Some(true) =>
      System.err.println("SUCCESS: All the goals were successfully proved")
    case Some(false) =>
      System.err.println("FAIL: I was unable to prove some of the goals")
      System.exit(1)
    case _ =>
  }
  
  def mainBool(args: Array[String]): Option[Boolean] = {
    val conf = new Conf(args)
    val graph = new MainHypergraphImplementation(conf)
    
    if(conf.gui()) {
      graph.pausable = true
      graph.launchGUI()
    }
    
    if(conf.gen())
      graph.autoTransformations ::= graph.unshare(conf.arity())
      
    if(conf.noLetToId()) 
      graph.autoTransformations ::= 
        PartialFunction(biHProc2HProc(partFun2BiHProc(graph.letToId)))
      
//    if(conf.noLetReduce()) 
//      graph.autoTransformations ::= 
//        PartialFunction(biHProc2HProc(partFun2BiHProc(graph.letVar)))
   
    if(conf.noAutoReduce()) 
      graph.autoTransformations ::= 
        PartialFunction(biHProc2HProc(partFun2BiHProc(graph.caseReduce)))
     
    val output = conf.output.get.fold(System.out)(o => new java.io.PrintStream(o))
        
    // read the file
    val preprog = ProgramParser.parseFile(conf.file()).resolveUnbound()
    if(conf.reformat.isDefined) {
      Console.withOut(output)(
          Reformat(preprog.mergeAppsAndLambdas.topLevelEtaExpand, conf.reformat()))
      return None
    }
    val prog = if(conf.only()) preprog.removeUnreferenced.simplify() else preprog.simplify()
    val propdefs = collection.mutable.Set(prog.propdefs.toSeq:_*)
    prog.warnUndefined()
    prog.loadInto(graph)
    
    val postponedActions = collection.mutable.Stack[NamedNodes => Unit]()
    
    // load rewriting rules
    for(r <- prog.rules) r match {
      case RuleEq(e1,e2) =>
        graph.addPatternTrigger(e1, { (n,subst) =>
          graph.log("-- rule trigger fired: " + 
              graph.nodeToString(n) + " matches " + e1 + " -> " + e2)
          logSubst(graph, subst)
          
          if(subst.values.exists(_.deref.getVarUnused == Some(-1)))
            graph.log("-- discarded because one of the variables is unused")
          else
            postponedActions.push { g =>
              g.trans("user-specified rule") {
                graph.log("-- rewriting rule: " + 
                    graph.nodeToString(n) + " matches " + e1 + " -> " + e2)
                logSubst(graph, subst)
                g.glue(List(n, e2.loadInto(g, Map(), subst)))
              }
            }
          
          graph.log("")
        })
      case RuleGen(e) =>
        graph.addPatternTrigger(e, { (n,subst) =>
          graph.log("-- generalize trigger fired: " + graph.pretty(n) + " matches " + e)
          logSubst(graph, subst)
          
          if(subst.values.exists(_.deref.getVarUnused == Some(-1)))
            graph.log("-- discarded because one of the variables is unused")
          else
            postponedActions.push { g =>
              g.trans("user-specified generalization") {
                graph.log("-- generalization: " + graph.nodeToString(n) + " matches " + e)
                logSubst(graph, subst)
                val bnds = subst.keys.map(s => s -> ExprVar(s)).toList
                g.glue(List(n, ExprLet(e, bnds).loadInto(g, Map(), subst)))
              }
            }
          
          graph.log("")
        })
    }
    
    // load props we want to prove as goals
    val goals =
      if(conf.prove())
        prog.prove.map(_.loadAsGoal(graph))
      else Nil
      
    if(conf.prove() && goals.isEmpty) {
      System.err.println("--prove is supplied but there is nothing to prove")
      System.exit(2)
    }
      
    // Load terms we want to residualize
    val resid =
      if(conf.resid())
        prog.residualize.map(_.bindUnbound.loadInto(graph))
      else Nil
    
    // assign zero (co)depth to all initial nodes
    for(n <- graph.allNodes)
      graph.zeroBoth(n.deref)
      
    // load tests
    if(conf.test())
      prog.loadTestsInto(graph)
      
    // this function checks if all the goals has been proved
    def checktask() : Boolean = {
      if(conf.verbose())
        for((name, p) <- propdefs.toList; g <- p.asGoalWithoutLoading(graph)) {
          if(graph.checkGoal(g)) {
            System.err.println("!!! prop " + name + ": " + p)
            propdefs.remove((name, p))
          }
        }
      
      val achieved = goals.map(graph.checkGoal(_))

      achieved.nonEmpty && achieved.forall(_ == true)
    }
    
    // Perform traditional multiresult supercompilation (sort of)
    if(conf.supercompile())
      performTraditionalSupercompilation(graph, conf, goals, resid)
      
    // Perform graph transformation
    performTransformation(graph, conf, goals, checktask, postponedActions)
    
    // Perform residualization by testing
    if(conf.resid()) {
      // Even if there are cached results, they are of no use to us
      graph.clearRunCache()
      
      // Load (again) user-defined tests
      if(!conf.residAutoTestOnly())
        prog.loadTestsInto(graph)
      
      performResidualization(graph, conf, resid, output)
    }
    
      
    // Dump stuff
    if(conf.dumpDot()) println(graph.toDot)
    if(conf.dumpDot2()) println(graph.toDotLight)
    if(conf.dumpCode()) println(graph.toProg)
    
    if(conf.prove())
      Some(checktask())
    else
      None
  }
  
  /////////////////////////////////////////////////////////////////////////////////////////
  
  def performTraditionalSupercompilation(graph: MainHypergraphImplementation, conf: Conf,
                                         goals: List[GoalProp], resid: List[RenamedNode]) {
    if(conf.verbose())
      System.err.println("Supercompiling...")
      
    val s = new Supercompilation(graph, 30)
    val ns = graph.namedNodes.values.toList
    val fromgoals =
      for(g <- goals) yield g match {
        case GoalPropEqModuloRen(l, r) => List(l, r)
        case GoalPropEq(l, r) => List(l, r)
        case GoalPropReturnsConstr(l, _) => List(l)
      }
    
    for(r <- fromgoals.flatten ++ resid)
      s.supercompile(r.node)
  }
  
  /////////////////////////////////////////////////////////////////////////////////////////
  
  def performTransformation(graph: MainHypergraphImplementation, conf: Conf, 
                            goals: List[GoalProp], checktask_bool: () => Boolean,
                            postponedActions: collection.mutable.Stack[NamedNodes => Unit]) {    
    val maxarity = conf.arity()
    val maxdepth = conf.depth()
    val maxcodepth = conf.codepth()
    
    // This buffer stores all hyperedges that will be added to the graph
    val buffer = HyperBuffer(graph)
    val bufgraph = if(conf.nobuf()) graph else buffer
    // This buffer stores hyperedges for each transformation and makes sure
    // that no hyperedge exceeds the maximal arity
    val tr =
      if(conf.cheat())
        new PostFilter(bufgraph, h => h.arity <= maxarity) with Transformations
      else
        new PostFilter(bufgraph, h => h.used.size <= maxarity) with Transformations
      
    
    var generation = 0
    var stop = false
    
    def checktask() {
      stop ||= checktask_bool()
    }
    
    def stats() {
      if(conf.verbose()) {
        System.err.println("Generation: " + generation)
        System.err.println("Nodes: " + graph.allNodes.size)
        System.err.println("Hyperedges: " + graph.allHyperedges.size)
        System.err.println()
      }
    }
    
    def gendump() {
      if(conf.dumpGenCode.isSupplied) {
        val out = 
          new java.io.PrintWriter(
            new java.io.File(conf.dumpGenCode() + "-" + generation))
        try {
          out.write(graph.toProg)
        } finally { out.close() }
      }
    }
    
    stats()
    gendump()
    checktask()
    
    // main loop
    while(!stop && generation < conf.generations()) {
      graph.changed = false
      
      if(postponedActions.nonEmpty) {
        if(conf.verbose())
          System.err.println("Applying rules...")
        val list = postponedActions.toList
        postponedActions.clear()
        list.foreach(_(graph))
      }
      
      if(conf.verbose())
        System.err.println("Transforming...")  
          
      val trans =
        (if(conf.gen()) partFun2BiHProc(tr.letUp(maxarity)) else tr.transNone) &
        (if(conf.drive2()) tr.transDrive2 else tr.transDrive) &
        (if(conf.total()) tr.transTotal else tr.transUntotal) &
        (if(conf.noLetReduce()) bFun2BiHProc(tr.letVar) else tr.transNone)
      graph.transform(trans)
      buffer.commit()
      
      if(conf.verbose())
        System.err.println("Pairs processed: " + graph.lastPairsProcessed)
      
      // Pairwise generalization
      if(conf.genPair()) {
        val genr = new Generalizer()
        val gens =
          for(i <- graph.nodes.toList; j <- graph.nodes.toList; 
              if i.hashCode() < j.hashCode()) yield
            genr.generalizeNodes(i, j)
        
        val gs = gens.flatten.sortBy(-_.depth).filter(_.depth > 1)
        for(g <- gs) {
          graph.log("-- Pairwise generalization")
          g.toLog(graph)
          graph.log("")
          g.performGeneralization(graph)
        }
      }
      
      generation += 1
      
      checktask()
          
      if(!stop && !conf.noiso()) {
        if(conf.verbose())
          System.err.println("Computing likeness...")
        val nodes = graph.allNodes.toList
        //val likenesscalc = new ByTestingLikenessCalculator(graph)
        val likenesscalc = new DefaultLikenessCalculator(conf.total())
        //val likenesscalc = 
        //  new CachingLikenessCalculator(new DefaultLikenessCalculator(conf.total()))
        //val likenesscalc = new OldLikenessCalculator(true)//conf.total())
        lazy val like =
          (for(l <- nodes; r <- nodes; 
              if l.hashCode < r.hashCode;
              if conf.mergeUseless() || 
                    LikenessCalculator.notCompletelyUseless(l, r) ||
                    (graph.depths(l) == 0 && graph.depths(r) == 0);
              lkl <- likenesscalc.likenessN(l, r); if lkl._1 > 0) yield {
            val rl = 2 //LikenessCalculator.reverseLikeness(l, r)
            (rl,lkl,l,r)
          }) ++
          (for(l <- nodes; lkl <- likenesscalc.likenessN(l, l)) yield {
            val rl = 2 //LikenessCalculator.reverseLikeness(l, r)
            (rl,lkl,l,l)
          })
          
        lazy val user =
          goals.collect {
            case GoalPropEqModuloRen(l, r) => (1, (1, Renaming()), l.node, r.node)
            case GoalPropEq(l, r) => (1, (1, Renaming()), l.node, r.node)
          }
        
        var eprover = new EquivalenceProver(graph, likenesscalc)
        
        if(conf.verbose())
          System.err.println("Performing merging by isomorphism...")
        val candidates =
          if(conf.onlyRequested()) user
          else like.toList.sortBy(p => -p._2._1)
        if(conf.verbose())
          System.err.println("Number of candidate pairs: " + candidates.size)
          
        for(((rl, (i,ren2),l1,r1), j) <- candidates.zipWithIndex; if !stop) {
          val lderef = l1.deref;
          val rderef = r1.deref;
          val l = lderef.node;
          val r = rderef.node;
          val ren1 = lderef.renaming.inv comp ren2 comp rderef.renaming;
          
          for(ren <- if(!(l ~~ r)) List(ren1) else likenesscalc.viablePermutations(l)) {
            val lpretty = l.prettyDebug
            val rpretty = r.prettyDebug
            if(conf.verbose()) {
//              System.err.println("==Trying to prove== likeness: " + i + " idx: " + j)
//              System.err.println("== reverse likeness: " + likenesscalc.reverseLikeness(l, r))
//              System.err.println(lpretty)
//              System.err.println("=======================" + ren)
//              System.err.println(rpretty)
//              System.err.println("=======================\n")
              //System.err.println(eq)
            }
            val eq = eprover.prove(l.deref.node, r.deref.node, ren)
            if(eq != None) {
              if(conf.verbose()) {
                System.err.println("==These two are equal== likeness: " + i + " idx: " + j)
                //System.err.println("== reverse likeness: " + likenesscalc.reverseLikeness(l, r))
                System.err.println(lpretty)
                System.err.println("=======================" + eq.get.renaming)
                System.err.println(rpretty)
                System.err.println("=======================\n")
                //System.err.println(eq)
//                def printstuff(t: EqProofTree) {
//                  System.err.println("---- reverse likeness: " + 
//                    likenesscalc.reverseLikeness(t.nodes._1, t.nodes._2))
//                  System.err.println(t.nodes._1.prettyDebug)
//                  System.err.println("----------------------" + eq.get.renaming)
//                  System.err.println(t.nodes._2.prettyDebug)
//                  System.err.println("----------------------\n")
//                  t.out.foreach(_._3.foreach(printstuff(_)))
//                }
//                printstuff(eq.get)
              }
              graph.log("-- Proved by isomorphism")
              eq.get.toLog(graph)
              graph.log("")
              eq.get.performGluing(graph)
              val st = eprover.stats
              eprover = new EquivalenceProver(graph, likenesscalc)
              eprover.stats = st
              //likenesscalc.cached.clear()
              checktask()
            }
          }
        }
        
        if(!stop && conf.driveRecommended() > 0) {
          if(conf.verbose())
              System.err.println("Driving recommended nodes")
          val stats = eprover.stats 
          for(n <- stats.toList.filter 
                { case ((l,r),_) => 
                    likenesscalc.likeness(l.deref, r.deref).map(_._1) == Some(0) }
                .sortBy(-_._2).take(conf.driveRecommended())
                .flatMap(p => List(p._1._1, p._1._2)).map(_.deref.node).distinct) {
            graph.drive(n)
          }
        }
      }
      
      stats()
      gendump()
      checktask()
      
//      println("hypers: " + graph.allHyperedges.size)
//      println("Counting circuits...")
//      val circs =
//        Circuit.circuits[GuardedOrStructural](graph)
//            .map(Circuit.norm(_)).map(c => (c.head._1, Circuit.backbone(c)))
//      println("Circuits: " + circs.length)
//      val mostcomm = circs.groupBy(_._2).maxBy(_._2.length)
//      println("Most common backbone: " + mostcomm._1 + " " + mostcomm._2.length)
//      println(mostcomm._2.map(x => x._1.prettyDebug).mkString("\n"))
      
      if(!graph.changed)
        stop = true
    }
    
    if(conf.stat()) {
      println("#nodes " + graph.allNodes.size)
      println("#hyperedges " + graph.allHyperedges.size)
      println("#generations " + generation)
    }
  }
  
  /////////////////////////////////////////////////////////////////////////////////////////
  
  // residualization (by testing)
  def performResidualization(graph: MainHypergraphImplementation, conf: Conf,
                             resid: List[RenamedNode], output: java.io.PrintStream) {
    val residlist =
      if(resid.nonEmpty) resid.map(_.deref)
      else graph.namedNodes.values.toList.map(_.deref)
    
    if(conf.verbose())
      System.err.println("Residualizing...")
    
    graph.residualizing = true
      
    
    
    val residualizer = ByTestingResidualizer(graph, conf.residAutoTest.get.getOrElse(1)) 
    
    if(conf.residAutoTest.isSupplied) {
      if(conf.verbose())
        System.err.println("Autogenerating and running tests...")
      for(n <- residlist)
        residualizer.autoTest(n.node)
    }
    
    if(conf.verbose())
      System.err.println("Done running tests...")
      
    val subgraphs = residualizer(residlist)
    if(conf.verbose())
      System.err.println("Residual graphs count: " + subgraphs.size)
    if(subgraphs.nonEmpty) {
      val res = subgraphs.minBy(_.nodes.size)
      
      if(conf.stat()) {
        println("#residual-nodes " + res.nodes.size)
        println("#residual-hyperedges " + res.hyperedges.size)
      }
      
      val to_resid = 
        graph.namedNodes.filter(x => res.nodes(x._2.deref.node)) ++ 
        resid.zipWithIndex.map { case (r,i) => ("residual_" + i, r) }
        
      Console.withOut(output)(
        Reformat(ToProgram(graph, to_resid, res), "speed-benchmark"))
      
//      for((n,h) <- res.hyperedges 
//          if !n.isVar && !n.definingHyperedge.exists(
//                            h => h.label.isInstanceOf[Construct] && h.dests.isEmpty )) {
//        println("depth: " + graph.depth(h.source) + " codepth: " + graph.codepth(h.source))
//        println("maxcost: " + (-1.0 :: graph.runCache(h.source.node).map(_._2.cost).toList).max)
////        for((a,r) <- graph.runCache(h.source.node)) {
////          println(a.mkString(" ") + "\t" + r.cost)
////        }
//        println(graph.nodeFunName(h.source) + " =\n" +
//            indent(graph.prettyHyperedge(h, graph.nodeFunName), "  ") + ";\n")
//      }
//      for((name,n) <- graph.namedNodes if res.nodes.contains(n.deref.node)) {
//        val dern = n.deref
//        val r = graph.nodeFunName(dern)
//        val l = name + graph.prettyRename(dern.renaming,
//                    (0 until dern.node.arity).map("v" + _ + "v").mkString(" ", " ", ""))
//        if(l != r)
//          println(l + " = " + r + ";")
//      }
    }
  }

}