package graphsc
import graphsc._
import app._
import transformation._
import residualization._

object TestDebug {
  import Samples._
  
  
  def info(s: String) = println(s)
  def test(s: String)(code: => Unit) = code
  
  def main(args: Array[String]) {
    def cmd(s: String) {
      test("eqprover " + s) {
        assert(EqProverApp.mainBool(s.split(" +")) == Some(true))
      }
    }
    
    cmd("-p -v -c10 -d10 -a4 --nogen  samples/idle")
    //cmd("-p -v --super -c3 -d3 -a5 samples/map-comp")
    //cmd("--prove --integrity-check --test ./samples/even-double")
    //cmd("-g0 -v --resid nrev nrev-code-dump")
  }

}