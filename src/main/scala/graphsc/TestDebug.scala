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
        assert(EqProverApp.mainBool(s.split(" ")) == Some(true))
      }
    }
    
    cmd("-tauto -v -g5 --nogen samples/exp-idle")
  }

}