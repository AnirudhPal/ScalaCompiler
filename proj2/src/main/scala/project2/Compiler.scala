package project2

import scala.collection.mutable.HashMap

/*
 * This compiler generates Scala-like code that computes the
 * result of a given AST.
 */
abstract class StackCompiler extends BugReporter with Codegen {
  import Language._

  type Loc = Int

  // Pretty printing
  var nTab = 0
  def emitln(msg: String): Unit = emitln(msg, nTab)

  /**
   * Env of the compiler. Keeps track of the location
   * in memory of each variable defined.
   */
  class Env {
    def undef(name: String) = BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")

    def apply(name: String): Loc = undef(name)
  }

  case class LocationEnv(
    vars: Map[String, Loc] = Map.empty,
    outer: Env = new Env) extends Env {

      /*
       * Return a copy of the current state plus a
       * variable 'name' at the location 'loc'
       */
      def withVal(name: String, loc: Loc): LocationEnv = {
        copy(vars = vars + (name -> loc))
      }

      /*
       * Return the location of the variable 'name'
       */
      override def apply(name: String): Loc = vars.get(name) match {
        case Some(loc) => loc
        case _  =>outer(name)
      }
  }

  /*
   * Generate code that computes the unary operator
   * 'op' on the value at memory location 'sp' and that
   * stores the result at 'sp'.
   */
  def transUn(op: String)(sp: Loc) = op match {
    case "-" => emitln(s"memory($sp) = -memory($sp)")
    case "+" => ()
  }

  /*
   * Generate code that computes the binary operator
   * 'op' on the values at memory location 'sp' and
   * 'sp1' and that stores the result at 'sp'.
   */
  def transBin(op: String)(sp: Loc, sp1: Loc) = op match {
    case "-" => emitln(s"memory($sp) -= memory($sp1)")
    case "+" => emitln(s"memory($sp) += memory($sp1)")
    case "*" => emitln(s"memory($sp) *= memory($sp1)")
    case "/" => emitln(s"memory($sp) /= memory($sp1)")
    case "%" => emitln(s"memory($sp) %= memory($sp1)")
  }

  /*
   * Generate code that computes the binary operator
   * 'op' on the values at memory location 'sp' and
   * 'sp1' and that stores the result in 'flag'.
   */
  def transCond(op: String)(sp: Loc, sp1: Loc) = op match {
    case "==" => emitln(s"flag = (memory($sp) == memory($sp1))")
    case "!=" => emitln(s"flag = (memory($sp) != memory($sp1))")
    case "<=" => emitln(s"flag = (memory($sp) <= memory($sp1))")
    case ">=" => emitln(s"flag = (memory($sp) >= memory($sp1))")
    case "<"  => emitln(s"flag = (memory($sp) <  memory($sp1))")
    case ">"  => emitln(s"flag = (memory($sp) >  memory($sp1))")
  }

  /*
   * Generate code that computesS the result of the
   * computation represented by the AST 'exp'.
   */
  def emitCode(exp: Exp): Unit = {
    emitln("type Val = Int")
    emitln("val memory = new Array[Val](1000)")
    emitln("var flag = true")
    trans(exp, 0)(LocationEnv())
    emitln(s"memory(0)")
  }

  /*
   * Generate code that computes the result of the
   * computation represented by the AST 'exp'. The
   * value will be placed at memory location 'sp'
   */
  def trans(exp: Exp, sp: Loc)(env: LocationEnv): Unit = exp match {
    case Lit(x) =>
      emitln(s"memory($sp) = $x")
    case Unary(op, v) =>
      trans(v, sp)(env)
      transUn(op)(sp)
    case Prim(op, lop, rop) =>
      trans(lop, sp)(env)
      trans(rop, sp + 1)(env)
      transBin(op)(sp, sp + 1)
    case Let(x, a, b) =>
      trans(a, sp)(env)
      trans(b, sp + 1)(env.withVal(x, sp))
      emitln(s"memory($sp) = memory(${sp + 1})")
    case Ref(x) =>
      emitln(s"memory($sp) = memory(${env(x)})")
    case Cond(op, l, r) =>
      trans(l, sp)(env)
      trans(r, sp+1)(env)
      transCond(op)(sp, sp + 1)
    case If(cond, tBranch, eBranch) =>
      trans(cond, sp)(env) // Set flag value
      emitln(s"if (flag) {")
      nTab += 1
      trans(tBranch, sp)(env)
      nTab -= 1
      emitln("} else {")
      nTab += 1
      trans(eBranch, sp)(env)
      nTab -= 1
      emitln("}")
    case VarDec(x, rhs, body) =>
      trans(rhs, sp)(env)
      trans(body, sp + 1)(env.withVal(x, sp))
      emitln(s"memory($sp) = memory(${sp + 1})")
    case VarAssign(x, rhs) =>
      trans(rhs, sp)(env)
      emitln(s"memory(${env(x)}) = memory($sp)")
    case While(cond, lBody, body) =>
      trans(cond, sp)(env)
      emitln(s"while (flag) {")
      nTab += 1
      trans(lBody, sp)(env)
      trans(cond, sp)(env)
      nTab -= 1
      emitln(s"}")
      trans(body, sp)(env)
  }

}

abstract class X86Compiler extends BugReporter with Codegen {
  import Language._

  type Loc = Int
  type PhyLoc = String

  /**
   * Env of the compiler. Keep track of the location
   * in memory of each variable defined.
   */
  private class Env {
    def undef(name: String) = BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")

    def apply(name: String): Loc = undef(name)
  }

  private case class LocationEnv(
    vars: Map[String, Loc] = Map.empty,
    outer: Env = new Env) extends Env {

      /*
       * Return a copy of the current state plus a
       * variable 'name' at the location 'loc'
       */
      def withVal(name: String, loc: Loc): LocationEnv = {
        copy(vars = vars + (name -> loc))
      }

      /*
       * Return the location of the variable 'name'
       */
      override def apply(name: String): Loc = vars.get(name) match {
        case Some(loc) => loc
        case _  => outer(name)
      }
  }

  // List of available register.
  val regs = Seq("%rbx", "%rcx", "%rdi", "%rsi", "%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15")

  /*
   * Generate code that computes the unary operator
   * 'op' on the value at memory location 'sp' and that
   * stores the result at 'sp'.
   *
   * TODO: Fill in transUn with the appropriate code.
   */
  def transUn(op: String)(sp: Loc) = op match {
    case "+" =>
      emitln(s"nop")
    case "-" => 
      emitln(s"movq ${regs(sp)}, ${regs(sp+1)}")
      emitln(s"movq $$0, ${regs(sp)}")
      emitln(s"subq ${regs(sp+1)}, ${regs(sp)}") 
    case _ => BUG(s"Unary operator $op undefined")
  }

  /*
   * Generate code that computes the binary operator
   * 'op' on the values at memory location 'sp' and
   * 'sp1' and that stores the result at 'sp'.
   */
  def transBin(op: String)(sp: Loc, sp1: Loc) = op match {
    case "+" => emitln(s"addq ${regs(sp1)}, ${regs(sp)}")
    case "-" => emitln(s"subq ${regs(sp1)}, ${regs(sp)}")
    case "*" => emitln(s"imulq ${regs(sp1)}, ${regs(sp)}")
    case "/" =>
      emitln(s"movq ${regs(sp)}, %rax")
      emitln(s"cqto")
      emitln(s"idiv ${regs(sp1)}")
      emitln(s"movq %rax, ${regs(sp)}")
    case _ => BUG(s"Binary operator $op undefined")
  }

  type Label = String

  var nLabel = 0
  def freshLabel(pref: String) = { nLabel += 1; s"$pref$nLabel" }

  /*
   * Generate code that jumps to the label 'label'
   * if the CPU flags are verifying the operator
   * 'op'
   * NOTE: The flags will need to be set before.
   */
  def transJumpIf(op: String)(label: Label) = {
    op match {
      case "==" => emitln(s"je $label")
      case "!=" => emitln(s"jne $label")
      case "<=" => emitln(s"jle $label")
      case ">=" => emitln(s"jge $label")
      case "<"  => emitln(s"jl $label")
      case ">"  => emitln(s"jg $label")
    }
  }

  /*
   * Generate code that compute the result of the
   * computation represented by the AST 'exp'.
   */
  def emitCode(exp: Exp): Unit = {
    trans(exp, 0)(LocationEnv())
    emitln(s"movq ${regs(0)}, %rax")
  }

  /*
   * Generate code that compute the result og the
   * computation represented by the AST 'exp'. The
   * value will be placed at memory location 'sp'
   *
   * TODO: Fill in each () with the appropriate code.
   */
  def trans(exp: Exp, sp: Loc)(env: LocationEnv): Unit = exp match {
    case Lit(x) =>
      emitln(s"movq $$$x, ${regs(sp)}")
    case Unary(op, v) =>
      trans(v, sp)(env)
      transUn(op)(sp)
    case Prim(op, lop, rop) =>
      trans(lop, sp)(env)
      trans(rop, sp+1)(env)
      transBin(op)(sp, sp+1) 
    case Let(x, a, b) =>
      trans(a, sp)(env)
      trans(b, sp+1)(env.withVal(x, sp))
      emitln(s"movq ${regs(sp+1)}, ${regs(sp)}")
    case Ref(x) => 
      emitln(s"movq ${regs(env.apply(x))}, ${regs(sp)}")
    case Cond(op, l, r) => 
      trans(l, sp)(env)
      trans(r, sp+1)(env)
      emitln(s"cmpq ${regs(sp+1)}, ${regs(sp)}") 
    case If(cond, tBranch, eBranch) =>
      trans(cond, sp)(env)
      val lab = freshLabel("if")
      transJumpIf(cond.op)(s"${lab}_then")
      trans(eBranch, sp)(env)
      emitln(s"jmp ${lab}_end")
      emitln(s"${lab}_then:", 0)
      trans(eBranch, sp)(env)
      emitln(s"${lab}_end:", 0)
    case VarDec(x, rhs, body) =>
      trans(rhs, sp)(env)
      trans(body, sp+1)(env.withVal(x, sp))
      emitln(s"movq ${regs(sp+1)}, ${regs(sp)}")
    case VarAssign(x, rhs) =>
      trans(rhs, sp)(env)
      emitln(s"movq ${regs(sp)}, ${regs(env.apply(x))}")
    case While(cond, lBody, body) =>
      val lab = freshLabel("loop")
      emitln(s"jmp ${lab}_cond")
      emitln(s"${lab}_body:", 0)
      // TODO: continue ...
      trans(lBody, sp)(env)
      emitln(s"${lab}_cond:",0)
      trans(cond, sp)(env)
      transJumpIf(cond.op)(s"${lab}_body")
      trans(body, sp)(env)
  }
}
