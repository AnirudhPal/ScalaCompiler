package project2

import org.scalatest._
import java.io.{ByteArrayOutputStream, PrintWriter}

// Define the stream method
trait TestOutput {
  import Language._

  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class CompilerTest extends FunSuite {
  import Language._

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  def testCompiler(ast: Exp, res: Int) = {
    val interpreter = new X86Compiler with TestOutput

    val code = interpreter.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("arithm") {
    testCompiler(Lit(-21), -21)
    testCompiler(Prim("-", Lit(10), Lit(2)), 8)
  }

  test("All") {
    testCompiler(VarDec("x",Lit(10),VarDec("b",Lit(1),While(Cond(">",Ref("x"),Ref("b")),If(Cond("==",Ref("b"),Lit(5)),Ref("b"),VarAssign("b",Prim("+",Ref("b"),Lit(7)))),Ref("b")))), 15)
  }  
}
