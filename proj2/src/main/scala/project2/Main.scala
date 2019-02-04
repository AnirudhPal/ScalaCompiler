package project2

import java.io._
import scala.io._

trait CodeGenerator {
  // Define the PrintWriter used to emit
  // the code.
  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut

  def emitCode(ast: Language.Exp): Unit

  // Emit the code and return the String
  // representation of the code
  def code(ast: Language.Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

object Runner {
  import Language._

  def main(args: Array[String]): Unit = {

    val src = if (new File(args(0)).exists) {
      val source = Source.fromFile(args(0))
      try source.getLines mkString "\n" finally source.close()
    } else {
      args(0)
    }

    println("============ SRC CODE ============")
    println(src)
    println("==================================\n")

    val reader = new BaseReader(src, '\u0000')
    val scanner = new Scanner(reader)

    // Parser to test!
    // TODO: Change this as you finish parsers
    val parser = new LoopParser(scanner)
    val ast = parser.parseCode

    println("============= AST ================")
    println(s"\t$ast")
    println("==================================\n")

    val analyzer = new SemanticAnalyzer(parser)
    println("======= Semantic Analyzer ========")
    val (numWarning, numError) = analyzer.run(ast)
    println("==================================\n")
    if (numError > 0)
      return

    val interpreter = if (args.contains("st_int"))
      new StackInterpreter
    else
      new ValueInterpreter
    println("========== Interpreter ===========")
    println(s"Result ${interpreter.run(ast)}")
    println("==================================\n")

    // Generator to test
    if (args.contains("x86")) {
      val generator = new X86Compiler with CodeGenerator
      val code = generator.code(ast)

      val data = Map[Char,Int]()
      val runner = new ASMRunner(code, data)

      println("============ OUTPUT ==============")
      println(runner.code)
      println("==================================\n")

      if (runner.assemble != 0) {
        println("Compilation error!")
      } else {
        println("============ RESULT ==============")
        println(s"Result ${runner.run}")
        println("==================================")
      }
    } else {
      val generator = new StackCompiler with CodeGenerator
      val code = generator.code(ast)

      println("============ OUTPUT ==============")
      println(code)
      println("==================================")
    }

  }
}
