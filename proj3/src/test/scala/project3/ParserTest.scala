package project3

import java.io._
import org.scalatest._

class ParserTest extends FunSuite {
  import Language._

  def scanner(src: String) = new Scanner(new BaseReader(src, '\u0000'))

  def testBaseParser(op: String, res: Exp) = {
    val gen = new BaseParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == LetRec(Nil, res), "Invalid result")
  }

	// Add My Parsers
	def testSyntacticSugarParser(op: String, res: Exp) = {
    val gen = new SyntacticSugarParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == LetRec(Nil, res), "Invalid result")
  }

	def testFunctionParser(op: String, res: Exp) = {
    val gen = new FunctionParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == LetRec(Nil, res), "Invalid result")
  }

	def testArrayParser(op: String, res: Exp) = {
    val gen = new ArrayParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == LetRec(Nil, res), "Invalid result")
  }
	
	


  test("SingleDigit") {
    testBaseParser("1", Lit(1))
  }

  test("GenericPrecedence") {
    testBaseParser("2-4*3", Prim("-", List(Lit(2), Prim("*", List(Lit(4), Lit(3))))))
  }

  test("ParseType") {
    testBaseParser("val x: Int = 1; 2", Let("x", IntType, Lit(1), Lit(2)))
  }

  test("ParseOptionalType") {
    testBaseParser("val x = 1; 2", Let("x", UnknownType, Lit(1), Lit(2)))
  }

	// My Tests
	test("IfSugar") {
		testSyntacticSugarParser("if (1 == 1) 20 else 10; 10/5", Let("x$1", UnknownType, If(Prim("==", List(Lit(1), Lit(1))), Lit(20), Lit(10)), Prim("/", List(Lit(10), Lit(5)))))
	}

	test("DumSugar") {
		testSyntacticSugarParser("var x = 10; x = x + 10; x + 10", VarDec("x", UnknownType, Lit(10), Let("x$1", UnknownType, VarAssign("x", Prim("+", List(Ref("x"), Lit(10)))), Prim("+", List(Ref("x"), Lit(10))))))
	}
}
