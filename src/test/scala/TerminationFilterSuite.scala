import graphsc._
import residualization._
import interpretation._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class TerminationFilterSuite extends FunSuite {
  import Samples._
  
  test("These functions should pass the termination filter") {
    val g = 
      new TheHypergraph 
        with NamedNodes
        with IntegrityCheckEnabled
        
    val samples: List[((String, String), List[Value])] =
      List(
        Samples("add") -> List(3, 4),
        Samples("padd") -> List(3, 4),
        Samples("mul") -> List(3, 4),
        Samples("fib") -> List(5),
        ("fib1", 
        "fib1 x = case x of {Z -> Z; S y -> case y of {Z -> S Z; S x -> add (fib1 y) (fib1 x)}}")
          -> List(5),
        Samples("fac") -> List(3),
        Samples("add3Left") -> List(2, 4, 3),
        Samples("add3Right") -> List(3, 2, 1),
        Samples("ackermann") -> List(3, 3),
        Samples("id") -> List(5),
        Samples("reverseTo") -> List(5, 0),
        Samples("nrev") -> List(5),
        Samples("constz") -> List(5))
    
    val nodes_args = 
      for(((name, code), args) <- samples) yield {
        ProgramParser(g, code)
        (name, g(name), args)
      }
        
    for((name, n, args) <- nodes_args) {
      info("testing " + name)
      val res = HyperRunner.run(n, args)
      val g_filtered = new TheHypergraph with IntegrityCheckEnabled
      val tf = new TerminationFilter(g_filtered)
      val Some(n_filtered) = tf(n)
      assert(HyperRunner.run(n_filtered, args) === res)
    }
  }
  
  test("These guys won't pass the filter") {
    val g = 
      new TheHypergraph 
        with NamedNodes
        with IntegrityCheckEnabled
        
    ProgramParser(g, Samples.get("id"))
    ProgramParser(g, "badconst x y = case (id Z) of {Z -> x; S k -> y}")
    val samples =
      List(
        // pmul swaps arguments, so it cannot pass this filter (yet?)
        Samples.get("pmul"),
        // the proplem with "loop = loop" is that it wouldn't have outgoing hyperedges
        "loop = loop",
        "cloop = case C of {C -> cloop}",
        "grow x = grow (S x)",
        "constloop = badconst Z constloop",
        "oneOrZero = case oneOrZero of {Z -> Z; S x -> S Z}",
        // well, this function can be evaluated (if a lazy evaluator is used)
        // It looks strange because we should make sure that 
        // the caseof is not optimized out by the edge normalizer
        "two x = S (case (two (S x)) of {Z -> Z; S x -> S Z})"
        )
    
    val nodes = 
      for(code <- samples) yield {
        ProgramParser(g, code)
        val name = code.takeWhile(_ != ' ')
        (name, g(name))
      }
        
    for((name, n) <- nodes) {
      info("testing " + name)
      val g_filtered = new TheHypergraph with IntegrityCheckEnabled
      val tf = new TerminationFilter(g_filtered)
      assert(tf(n) === None)
    }
  }
}