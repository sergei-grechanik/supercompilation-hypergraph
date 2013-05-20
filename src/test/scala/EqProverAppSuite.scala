import graphsc._
import app._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class EqProverAppSuite extends FunSuite {
  
  def cmd(s: String) {
    test("eqprover " + s) {
      assert(EqProverApp.mainBool(s.split(" ")) === Some(true))
    }
  }
  
  cmd("-tauto --integrity-check --test -a4 ./samples/add-assoc")
  cmd("-tauto --integrity-check --test ./samples/dummy")
  cmd("-tauto --integrity-check --test ./samples/even-double")
  cmd("-tauto --integrity-check --test ./samples/idle")
  cmd("-tauto --integrity-check --test --nogen ./samples/idle")
  // --test fails on quad-idle, --integrity-check takes too long
  cmd("-tauto ./samples/quad-idle")
  cmd("-tauto --integrity-check --nogen ./samples/quad-idle")
  // takes too long without --nogen
  cmd("-tauto --integrity-check --nogen ./samples/exp-idle")
  cmd("-tauto --integrity-check --test ./samples/inf")
  cmd("-tauto --integrity-check --test ./samples/map-comp")
  cmd("-tauto --integrity-check --test --nogen ./samples/shuffled-let")
  
}
