package project1

import java.io._
import org.scalatest._

// Define the stream method
trait TestOutput {
  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class StackGeneratorTest extends FunSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testStackASMGenerator(ast: Exp, res: Int) = {
    val gen = new StackASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("SingleDigit") {
    testStackASMGenerator(Lit(2), 2)
  }

  // TODO more tests
  test("SingleAdd") {
    testStackASMGenerator(Plus(Lit(9), Lit(3)), 12)
  }
  test("SingleSub") {
    testStackASMGenerator(Minus(Lit(9), Lit(3)), 6)
  }
  test("MultipleAdd") {
    testStackASMGenerator(Plus(Plus(Plus(Plus(Plus(Plus(Plus(Plus(Lit(1),Lit(2)),Lit(3)),Lit(4)),Lit(5)),Lit(6)),Lit(7)),Lit(8)),Lit(9)), 45)
  }
  test("MultipleSub") {
    testStackASMGenerator(Minus(Minus(Minus(Minus(Minus(Minus(Minus(Minus(Lit(9),Lit(8)),Lit(7)),Lit(6)),Lit(5)),Lit(4)),Lit(3)),Lit(2)),Lit(1)), -27)
  }
  test("MultipleAddOp") {
    testStackASMGenerator(Minus(Plus(Minus(Plus(Minus(Plus(Minus(Plus(Lit(9),Lit(8)),Lit(7)),Lit(6)),Lit(5)),Lit(4)),Lit(3)),Lit(2)),Lit(1)), 13)
  }
  test("SingleMul") {
    testStackASMGenerator(Times(Lit(5), Lit(7)), 35)
  }
  test("SingleDiv") {
    testStackASMGenerator(Div(Lit(8), Lit(2)), 4)
  }
  test("MultipleMul") {
    testStackASMGenerator(Times(Times(Times(Lit(2),Lit(4)),Lit(6)),Lit(8)), 384)
  }
  test("MultipleDiv") {
    testStackASMGenerator(Div(Div(Lit(9),Lit(3)),Lit(2)), 1)
  }
  test("MultipleArith") {
    testStackASMGenerator(Plus(Plus(Minus(Lit(0),Lit(1)),Lit(2)),Div(Times(Lit(1),Lit(5)),Lit(2))), 3)
  }
  test("MultipleArithPara") {
    testStackASMGenerator(Minus(Times(Lit(7),Div(Plus(Lit(3),Lit(2)),Lit(2))),Lit(8)), 6)
  }
}

class RegGeneratorTest extends FunSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testRegASMGenerator(ast: Exp, res: Int) = {
    val gen = new RegASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("SingleDigit") {
    testRegASMGenerator(Lit(2), 2)
  }

  // TODO more tests
  test("SingleAdd") {
    testRegASMGenerator(Plus(Lit(9), Lit(3)), 12)
  }
  test("SingleSub") {
    testRegASMGenerator(Minus(Lit(9), Lit(3)), 6)
  }
  test("MultipleAdd") {
    testRegASMGenerator(Plus(Plus(Plus(Plus(Plus(Plus(Plus(Plus(Lit(1),Lit(2)),Lit(3)),Lit(4)),Lit(5)),Lit(6)),Lit(7)),Lit(8)),Lit(9)), 45)
  }
  test("MultipleSub") {
    testRegASMGenerator(Minus(Minus(Minus(Minus(Minus(Minus(Minus(Minus(Lit(9),Lit(8)),Lit(7)),Lit(6)),Lit(5)),Lit(4)),Lit(3)),Lit(2)),Lit(1)), -27)
  }
  test("MultipleAddOp") {
    testRegASMGenerator(Minus(Plus(Minus(Plus(Minus(Plus(Minus(Plus(Lit(9),Lit(8)),Lit(7)),Lit(6)),Lit(5)),Lit(4)),Lit(3)),Lit(2)),Lit(1)), 13)
  }
  test("SingleMul") {
    testRegASMGenerator(Times(Lit(5), Lit(7)), 35)
  }
  test("SingleDiv") {
    testRegASMGenerator(Div(Lit(8), Lit(2)), 4)
  }
  test("MultipleMul") {
    testRegASMGenerator(Times(Times(Times(Lit(2),Lit(4)),Lit(6)),Lit(8)), 384)
  }
  test("MultipleDiv") {
    testRegASMGenerator(Div(Div(Lit(9),Lit(3)),Lit(2)), 1)
  }
  test("MultipleArith") {
    testRegASMGenerator(Plus(Plus(Minus(Lit(0),Lit(1)),Lit(2)),Div(Times(Lit(1),Lit(5)),Lit(2))), 3)
  }
  test("MultipleArithPara") {
    testRegASMGenerator(Minus(Times(Lit(7),Div(Plus(Lit(3),Lit(2)),Lit(2))),Lit(8)), 6)
  }
}
