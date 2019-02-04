package project1

import java.io._
import org.scalatest._

class ParserTest extends FunSuite {

  def reader(src: String) = new BaseReader(src.iterator, '\u0000')
  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  test("SingleDigit") {
    val gen = new SingleDigitParser(reader("4"))
    val ast = gen.parseCode

    assert(ast == Lit(4), "Invalid result")
  }

  // Function Helper for SingleAddOpParser
  def testSingleAdd(op: String, res: Exp) = {
    val gen = new SingleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("SingleAddopAdd") {
    testSingleAdd("1+1", Plus(Lit(1),Lit(1)))
  }

  // Function Helper for MultipleAddOpParser
  def testMultipleAdd(op: String, res: Exp) = {
    val gen = new MultipleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("MultipleAddopAdd") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("1+2", Plus(Lit(1), Lit(2)))
    testMultipleAdd("1+2+3", Plus(Plus(Lit(1), Lit(2)),Lit(3)))
  }

  // TODO
  test("MultipleAddopSub") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("1-2", Minus(Lit(1), Lit(2)))
    testMultipleAdd("1-2-3", Minus(Minus(Lit(1), Lit(2)),Lit(3)))
  }

  test("MultipleAddop") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("1+2", Plus(Lit(1), Lit(2)))
    testMultipleAdd("1-2+3", Plus(Minus(Lit(1), Lit(2)),Lit(3)))
  }

  // Function Helper for ArithOpParser
  def testArith(op: String, res: Exp) = {
    val gen = new ArithOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  // TODO
  test("ArithOpMul") {
    testArith("1", Lit(1))
    testArith("1*2", Times(Lit(1), Lit(2)))
    testArith("1*2*3", Times(Times(Lit(1), Lit(2)),Lit(3)))
  }

  test("ArithOpDiv") {
    testArith("1", Lit(1))
    testArith("1/2", Div(Lit(1), Lit(2)))
    testArith("1/2/3", Div(Div(Lit(1), Lit(2)),Lit(3)))
  }

  test("ArithOp") {
    testArith("1", Lit(1))
    testArith("1/2", Div(Lit(1), Lit(2)))
    testArith("1*2-3+4/5", Plus(Minus(Times(Lit(1), Lit(2)), Lit(3)), Div(Lit(4), Lit(5))))
  }

  // Function Helper for ArithParOpParser
  def testArithPar(op: String, res: Exp) = {
    val gen = new ArithParOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  // TODO
  test("ArithPar") {
    testArithPar("(2*-(3+1))/3", Div(Times(Lit(2), Minus(Lit(0), Plus(Lit(3), Lit(1)))), Lit(3)))
  }
}
