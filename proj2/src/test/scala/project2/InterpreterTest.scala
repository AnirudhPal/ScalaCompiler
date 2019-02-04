package project2

import org.scalatest._

class InterpretTest extends FunSuite {
  import Language._

  def testInterpreter(ast: Exp, res: Int) = {
    val interpreter = new StackInterpreter

    assert(res == interpreter.run(ast), "Interpreter does not return the correct value")
  }

  test("arithm") {
    testInterpreter(Lit(-21), -21)
    testInterpreter(Prim("-", Lit(10), Lit(2)), 8)
  }

  test("All") {
    testInterpreter(VarDec("x",Lit(10),VarDec("b",Lit(1),While(Cond(">",Ref("x"),Ref("b")),If(Cond("==",Ref("b"),Lit(5)),Ref("b"),VarAssign("b",Prim("+",Ref("b"),Lit(7)))),Ref("b")))), 15)
  }
}
