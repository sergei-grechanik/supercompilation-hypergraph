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
  val residTestsSubset = opt[Int](noshort = true, descr = 
    "Actually run <arg> tests on the full graph, other tests will be used for testing")
  val residAutoTest = opt[Int](noshort = true, descr = 
    "Run <arg> smallest automatically generated tests for each node on the defining boundary")
  // val residAutoTestOnly = opt[Boolean](noshort = true, descr =
  //   "Don't use user-specified tests for residualization")
  val residTestResults = opt[Boolean](noshort = true, 
    descr = "Show test results for the residual program")
  val residSpeedupGraph = opt[Boolean](noshort = true, 
    descr = "Print (input size, speedup) graph")
    
  val only = opt[Boolean](noshort = true, descr = "Don't load unreferenced functions into graph")

  val params = opt[String](noshort = true, 
      descr = "File with additional parameters")
  
  val maxtrans = opt[Int](default = Some(Int.MaxValue), 
      descr = "Maximal number of transformations per generation")
  val arity = opt[Int](default = Some(10), descr = "Maximal arity of nodes")
  val depth = opt[Int](default = Some(10), descr = "Depth limit")
  val codepth = opt[Int](default = Some(10), descr = "Codepth limit")
  val gen = opt[Boolean](noshort = true, descr = "Enable unordered generalization")
  val noiso = opt[Boolean](noshort = true, descr = "Disable merging by bisimulation")
  val generations = opt[Int](default = Some(1000), descr = "Maximal number of generations")
  val naiveMrsc = opt[Boolean](noshort = true, descr = "Perform naive multi-result sc")
  val weakMerging = opt[Boolean](noshort = true, descr = "Disable merging up to renaming")
  val noLetToId = opt[Boolean](noshort = true, descr = "Disable destructive let to id conversion")
  val noLetUnused = opt[Boolean](noshort = true, descr = "Disable destructive let-unused")
  val noLetReduce = opt[Boolean](noshort = true, descr = "Disable destructive let-var reduction")
  val noAutoReduce = opt[Boolean](noshort = true, descr = "Disable destructive reduction")
  val noCaseInj = opt[Boolean](noshort = true, descr = "Disable case injectivity")
  val noConsInj = opt[Boolean](noshort = true, descr = "Disable constructor injectivity")
  val noCaseVar = opt[Boolean](noshort = true, descr = "Disable positive info propagation")
  val noDestrNorm = opt[Boolean](noshort = true, descr = "Disable destructive normalization")
  val autoPositivePropagation = 
    opt[Boolean](noshort = true, descr = "Positive info propagation until saturation")
  val genPair = opt[Boolean](noshort = true, 
      descr = "Traditional pairwise generalization")
  val supercompile = opt[Int](name = "super", default = None,
      descr = "Traditional multiresult supercompilation with this depth")
  val mergeUseless = opt[Boolean](noshort = true, 
      descr = "Merge nodes by iso even if that won't lead to more node merging")
  val onlyRequested = opt[Boolean](noshort = true, 
      descr = "Merge by iso only nodes specified in props")
  val drive2 = opt[Boolean](noshort = true, descr = "Use the second set of driving rules")
  val notrans = opt[Boolean](noshort = true, descr = "Disable ordinary transformations")
  val nobuf = opt[Boolean](noshort = true, 
      descr = "Disable bufferization of new edges, may lead to unstable behaviour")
  val sortCandidatesBackwards = opt[Boolean](noshort = true, 
      descr = "Sort candidate pairs in reverse order")
  val noCache = opt[Boolean](noshort = true, 
      descr = "Don't use cache during merging by bisimulation")
  val cse = opt[Boolean](noshort = true, 
      descr = "Perform common subexpression elimination")
  val fusion = opt[Boolean](noshort = true, 
      descr = "Perform loop fusion (tupling)")
  
  val dumpDot = opt[Boolean](noshort = true, descr = "Dump the graph to stdout")
  val dumpDot2 = opt[Boolean](noshort = true, descr = "Dump the graph to stdout (lightweight)")
  val dumpCode = opt[Boolean](noshort = true, 
    descr = "Dump the graph to stdout in a form of a program")
  val dumpGenCode = opt[String](noshort = true, 
    descr = "Dump code for each generation to files <arg>-i")
  val verbose = opt[Boolean](descr = "Be more verbose")
  val log = opt[Boolean](descr = "Log transformations to stdout")
  val stat = opt[Boolean](descr = "Print some statistics like number of nodes at the end")
  val showInterestingMergings = opt[Boolean](descr = "Print nodes merged by congruence or bisim")
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
        with HyperLogger {
  
  override val total = conf.total()
  override val integrityCheckEnabled = conf.integrityCheck()
  override val onTheFlyTesting = conf.test()
  override val prettifyingEnabled = !conf.nopretty()
  override val weakMerging = conf.weakMerging()
  override val autoLetToId = !conf.noLetToId()
  override val autoLetUnused = !conf.noLetUnused()
  override val autoLetReduce = !conf.noLetReduce()
  override val autoReduce = !conf.noAutoReduce()
  override val autoCaseInj = !conf.noCaseInj()
  override val autoConsInj = !conf.noConsInj()
  override val autoNormalize = !conf.noDestrNorm()
  override val naiveMrsc = conf.naiveMrsc()
  
  /*override def filterUpdatedPairs(pairs: List[(Hyperedge, Hyperedge)]): 
      List[(Hyperedge, Hyperedge)] =
    pairs.filter { 
      case (h1,h2) =>
        nodesOf(h1,h2).map(depths(_)).max <= conf.depth() &&
        nodesOf(h1,h2).map(codepths(_)).max <= conf.codepth()
    }*/
  
  var enableLoggingVar = conf.log()
  override def enableLogging = enableLoggingVar
  override val hyperTesterLogging = conf.testLog()

  override def beforeGlue(l: RenamedNode, r: Node) {
    if(conf.showInterestingMergings() && l.node.mouts.size > 1 && r.mouts.size > 1) {
      System.err.println("Interesting equivalence:")
      System.err.println(l.node.prettyDebug)
      System.err.println(r.prettyDebug)
      System.err.println()
    }
    super.beforeGlue(l, r)
  }
  
  
  var residualizing = false
  // override def limitFromMinCost(c: Double): Double =
  //   if(residualizing) Double.MaxValue
  //   else super.limitFromMinCost(c)
    
  // override def hyperedgeCost(h: Hyperedge): Double = 
  //   super.hyperedgeCost(h) * (1 + (depth(h.source) + codepth(h.source))*0.2)
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
    conf.verify()
    val graph = new MainHypergraphImplementation(conf)
    
    if(conf.gen())
      graph.autoTransformations ::= graph.unshare(conf.arity()).run
      
    if(conf.noLetToId()) 
      graph.autoTransformations ::= graph.letToId.runMono
        
    if(conf.noLetUnused()) 
      graph.autoTransformations ::= graph.letUnused.run
      
//    if(conf.noLetReduce()) 
//      graph.autoTransformations ::= 
//        PartialFunction(biHProc2HProc(partFun2BiHProc(graph.letVar)))
   
    if(conf.noAutoReduce()) 
      graph.autoTransformations ::= graph.caseReduce.runMono
        
//    if(conf.noDestrNorm())
//      graph.autoTransformations ::= graph.doNormalize
     
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

    if(conf.stat()) {
      println("#initial-nodes " + graph.nodes.size)
      println("#initial-hyperedges " + graph.allHyperedges.size)
    }
    
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

    // Generate tests for terms we want to residualize
    val resid_tests = 
      (if(conf.residAutoTest.isSupplied) {
        if(conf.verbose())
          System.err.println("Generating tests...")
        val residualizer = ByTestingResidualizer(graph)
        for(n <- resid; t <- residualizer.generateTests(n, conf.residAutoTest())) yield 
          (n, t)
      } 
      else Nil)

    // load user tests (without running)
    val user_tests = prog.loadTestsInto(graph)
    
    // assign zero (co)depth to all initial nodes
    for(n <- graph.allNodes)
      graph.zeroBoth(n.deref)
    
    val bench_results =
      if(conf.resid()) {
        for((n, as) <- user_tests ++ resid_tests) yield {
          val res = graph.runNode(n, as)
          (n, as, res.value, res.cost)
        }
      } else Nil

    graph.clearRunCache()

    // load tests
    if(conf.test())
      for((n, as) <- user_tests)
        graph.runNode(n, as)
      
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
    if(conf.supercompile.isDefined)
      performTraditionalSupercompilation(graph, conf, goals, resid)
      
    // Perform graph transformation
    performTransformation(graph, conf, goals, checktask, postponedActions)
    
    // Perform residualization by testing
    if(conf.resid()) {
      // Even if there are cached results, they are of no use to us
      graph.clearRunCache()

      if(conf.testLog())
        graph.enableLoggingVar = true

      performResidualization(graph, conf, resid, bench_results, output)
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
      
    val ns = graph.namedNodes.values.map(_.deref.node).toSet
    val s = new Supercompilation(graph, ns, conf.supercompile())
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
    
    def weak_merge() {
      // Should be safe even if weak merging is off (it's commutativity then)
      System.err.println("Ignoring regluing/commutativity in this version... ")
      return
      if(/*conf.weakMerging() &&*/ graph.changed) {        
        do {
          if(conf.verbose())
            System.err.print("Applying regluing/commutativity... ")  
          graph.changed = false
          val clone_trans = List(tr.anyId)
          graph.transform(clone_trans, 
              p => p._1.label.isInstanceOf[Id] || p._2.label.isInstanceOf[Id])
          if(conf.autoPositivePropagation())
            graph.transform(List(tr.caseVar), 
              p => p._1.label.isInstanceOf[CaseOf] || p._2.label.isInstanceOf[CaseOf], false)
          buffer.commit()
          if(conf.verbose())
            System.err.println(graph.allHyperedges.size + " hyperedges")
        } while(graph.changed)
        
        graph.changed = true
      }
    }
    
    weak_merge()
    stats()
    gendump()
    checktask()

    val trans =
        ((if(conf.gen()) List(tr.letUp(maxarity)) else Nil) ++
         (if(conf.notrans()) Nil else (
            (if(conf.drive2()) tr.transDrive2 else 
             if(conf.noCaseVar()) tr.transDriveNoCaseVar else tr.transDrive) ++
            (if(conf.total()) tr.transTotal else tr.transUntotal) ++
            (if(conf.noLetReduce()) List(tr.letVar) else Nil))))
        .map(_.modifyPriority((h1: Hyperedge, h2: Hyperedge, p: Double) => 
                                p + graph.depth(h1.source) + graph.codepth(h2.source)))
    
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
      
      if(!conf.supercompile.isDefined || generation > 0) {
        if(conf.verbose())
          System.err.println("Transforming...")  
            
        graph.enqueueTransformations(trans.map(_.modifyPriority((_, _, p) => p + generation)))
        graph.runPotentialTransformations(conf.maxtrans())
        graph.simplifyPotentialTransformations()
        buffer.commit()
        
        if(conf.verbose())
          System.err.println("Pairs processed: " + graph.lastPairsProcessed)
      } else {
        graph.changed = true
        System.err.println("Skipped transformations after supercompilation")  
      }

      if(conf.verbose())
          System.err.println("Removing holes...")
      graph.removeHoles()
      
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

      if(conf.fusion()) {
        val tupler = new Tupler(graph)
        tupler.printCandidates()
      }

      if(conf.cse()) {
        val cse = new CSE(graph)
        cse.performCSE()
      }
      
      checktask()
      
      if(!stop)
        weak_merge()
      
      generation += 1
      
      checktask()
          
      if(!stop && !conf.noiso()) {
        val likenesscalc = new DefaultLikenessCalculator(conf.total())
          
        lazy val user =
          goals.collect {
            case GoalPropEqModuloRen(l, r) => (1, (1, Renaming()), l.node, r.node)
            case GoalPropEq(l, r) => (1, (1, Renaming()), l.node, r.node)
          }

        if(conf.verbose())
          System.err.println("Performing merging by bisimilarity...")

        var changes_before = -1

        while(changes_before != graph.totalChanges) {
          changes_before = graph.totalChanges
          val aeqprover = new EquivalenceProverAdvanced(graph, likenesscalc)

          if (conf.onlyRequested())
            for (u <- user)
              aeqprover.tryMerging(u._3, u._4)
          else
            aeqprover.mergeAll()

          checktask()
          if (!stop)
            weak_merge()
        }
      }
      
      stats()
      gendump()
      checktask()
      
      if(!graph.changed && graph.potentialTransformations.isEmpty)
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
                             resid: List[RenamedNode], 
                             resid_tests: List[(RenamedNode, List[Value], Value, Double)],
                             output: java.io.PrintStream) {
    val residlist =
      if(resid.nonEmpty) resid.map(_.deref)
      else graph.namedNodes.values.toList.map(_.deref)
    
    if(conf.verbose())
      System.err.println("Residualizing...")
    
    graph.residualizing = true
      
    val tests_subset_size = conf.residTestsSubset.get.getOrElse(resid_tests.size)
    
    val residualizer = ByTestingResidualizer(graph, conf.residAutoTest.get.getOrElse(1)) 
    
    if(conf.verbose())
      System.err.println("Running tests...")
    for((n, vs, _, _) <- resid_tests.take(tests_subset_size))
      graph.runNode(n.deref, vs)
    if(conf.residTestResults()) {
      for(rn <- residlist) {
        val n = rn.deref.node
        System.err.println("\nTest results for node\n" + n.prettyDebug + "\n")
        for((args, v) <- graph.runCache(n).toList.sortBy(_._2.cost)) {
          System.err.println("[" + v.cost + "] " + args + " -> " + v.value)
        }
        System.err.println()
      }
    }

    val hardest_test = resid_tests.maxBy(_._4)

    val hardest_newres = graph.runNode(hardest_test._1, hardest_test._2)
    System.err.println("\nPredicted hardest speedup: " +
      (hardest_test._4 / hardest_newres.cost) + "\n")
    
    if(conf.verbose())
      System.err.println("Done running tests...")
      
    val subgraphs = residualizer(residlist)
    if(conf.verbose())
      System.err.println("Residual graphs count: " + subgraphs.size)
    if(subgraphs.nonEmpty) {
      val subgraph_evaluations =
        for(subgraph <- subgraphs.sortBy(_.nodes.size).take(300)) yield {
          // copying stuff from the original graph to a new graph for easier benchmarking
          val resid_graph = new TheHypergraph with HyperTester
          val nodesmap = collection.mutable.Map[Node, RenamedNode]()
          for(n <- subgraph.nodes)
            nodesmap += (n -> resid_graph.newNode(n.used))
          for(h <- subgraph.hyperedges.values) {
            val newh = 
              Hyperedge(h.label, 
                        h.source.renaming comp nodesmap(h.source.node),
                        h.dests.map(d => d.renaming comp nodesmap(d.node)))
            resid_graph.addHyperedge(newh)
          }

          val bench_results =
            for((n, vs, orig_res, base_cost) <- List(hardest_test)) yield {
              val d = n.deref
              val res = resid_graph.runNode(d.renaming comp nodesmap(d.node), vs)
              assert(orig_res == res.value)
              val new_cost = res.cost
              (n, vs, base_cost, new_cost, base_cost/new_cost)
            }

          (bench_results.head._5, subgraph, resid_graph, nodesmap)
        }

      val top = subgraph_evaluations.sortBy(x => (-x._1, x._2.nodes.size)).take(30)

      for(x <- top)
        println("hardest-speedup " + x._1 + " nodes " + x._2.nodes.size)

      val best = top.head
      val res = best._2
      val resid_graph = best._3
      val nodesmap = best._4
      
      if(conf.stat()) {
        println("#residual-nodes " + res.nodes.size)
        println("#residual-hyperedges " + res.hyperedges.size)
      }
      
      val to_resid = 
        graph.namedNodes.filter(x => res.nodes(x._2.deref.node)) ++ 
        resid.zipWithIndex.map { case (r,i) => ("residual_" + i, r) }
        
      // Console.withOut(output)(
      //   Reformat(ToProgram(graph, to_resid, res), "speed-benchmark"))

      System.err.println("\nResidual program:")
      System.err.println(ToProgram(graph, to_resid, res).toString)
      System.err.println("")

      val bench_results =
        for((n, vs, orig_res, base_cost) <- resid_tests) yield {
          val d = n.deref
          val res = resid_graph.runNode(d.renaming comp nodesmap(d.node), vs)
          assert(orig_res == res.value)
          val new_cost = res.cost
          (n, vs, base_cost, new_cost, base_cost/new_cost)
        }

      if(conf.residTestResults()) {
        for(rn <- residlist) {
          val n = nodesmap(rn.deref.node).node
          System.err.println("\nFinal results for node\n" + n.prettyDebug + "\n")
          for((args, v) <- resid_graph.runCache(n).toList.sortBy(_._2.cost)) {
            System.err.println("[" + v.cost + "] " + args + " -> " + v.value)
          }
          System.err.println()
        }
      }

      val speedups = bench_results.map(_._5).sorted

      if(conf.stat() && speedups.nonEmpty) {
        println("#worst-speedup " + speedups(0))
        println("#median-speedup " + speedups(speedups.length/2))
        println("#best-speedup " + speedups.last)
        println("#maxsize-speedup " + bench_results.sortBy(-_._2.map(_.size).sum).head._5)
        println("#hardest-speedup " + best._1)
      }

      if(conf.residSpeedupGraph()) {
        for((n, vs, base_cost, new_cost, speedup) <- bench_results)
          println(vs.map(_.size).sum + " " + speedup)
      }
    }

  }

}
