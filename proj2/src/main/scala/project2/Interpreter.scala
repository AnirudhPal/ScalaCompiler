package project2

import scala.collection.mutable.HashMap

abstract class Interpreter {
  type Val
  def run(ast: Language.Exp): Val
}

/**
 * This interpreter specifies the semantics of our
 * programming lanaguage.
 *
 * The evaluation of each node returns a value.
 */
class ValueInterpreter extends Interpreter with BugReporter {
  import Language._

  type Val = Int

  /**
   * Env of the interpreter. Keeps track of the value
   * of each variable defined.
   */
  class Env {
    def undef(name: String) =
      BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")

    def updateVar(name: String, v: Val): Val = undef(name)
    def apply(name: String): Val = undef(name)
  }

  case class BoxedVal(var x: Val)
  case class ValueEnv(
    vars: Map[String, BoxedVal] = Map.empty,
    outer: Env = new Env) extends Env {

      /*
       * Return a copy of the current state plus an immutable
       * variable 'name' of value 'v'
       */
      def withVal(name: String, v: Val): ValueEnv = {
        copy(vars = vars + (name -> BoxedVal(v)))
      }

      /*
       * Update the variable 'name' in this scope or in the
       * outer scope.
       * Return the new value of the variable
       */
      override def updateVar(name: String, v: Val): Val = {
        if (vars.contains(name))
          vars(name).x = v
        else
          outer.updateVar(name, v)
        v
      }

      /*
       * Return the value of the variable 'name'
       */
      override def apply(name: String): Val = {
        if (vars.contains(name))
          vars(name).x
        else
          outer(name)
      }
  }

  /*
   * Compute and return the result of the unary
   * operation 'op' on the value 'v'
   */
  def evalUn(op: String)(v: Val) = op match {
    case "-" => -v
    case "+" => v
  }

  /*
   * Compute and return the result of the binary
   * operation 'op' on the value 'v' and 'w'
   * Note: v op w
   */
  def evalBin(op: String)(v: Val, w: Val) = op match {
    case "-" => v-w
    case "+" => v+w
    case "*" => v*w
    case "/" => v/w
  }

  /*
   * Compute and return the result of the condition
   * operation 'op' on the value 'v' and 'w'
   * Note: v op w
   */
  def evalCond(op: String)(v: Val, w: Val) = op match {
    case "==" => v == w
    case "!=" => v != w
    case "<=" => v <= w
    case ">=" => v >= w
    case "<"  => v < w
    case ">"  => v > w
  }

  /*
   * Evaluate the AST starting with an empty Env
   */
  def run(exp: Exp) = eval(exp)(ValueEnv())

  /*
   * Evaluate the AST within the environment 'env'
   */
  def eval(exp: Exp)(env: ValueEnv): Val = exp match {
    case Lit(x) => x
    case Unary(op, v) =>
      evalUn(op)(eval(v)(env))
    case Prim(op, lop, rop) =>
      evalBin(op)(eval(lop)(env), eval(rop)(env))
    case Let(x, a, b) =>
      eval(b)(env.withVal(x, eval(a)(env)))
    case Ref(x) =>
      env(x)
    case If(Cond(op, l, r), tBranch, eBranch) =>
      if (evalCond(op)(eval(l)(env), eval(r)(env)))
        eval(tBranch)(env)
      else
        eval(eBranch)(env)
    case VarDec(x, rhs, body) =>
      eval(body)(env.withVal(x, eval(rhs)(env)))
    case VarAssign(x, rhs) =>
      env.updateVar(x, eval(rhs)(env))
    case While(Cond(op, l, r), lBody, body) =>
      while (evalCond(op)(eval(l)(env), eval(r)(env))) {
        eval(lBody)(env)
      }
      eval(body)(env)
  }

}

/**
 * This interpreter is a stack-based interpreter as we have seen
 * during the lecture.
 *
 * Rather than returning the value of a node, it stores it in memory,
 * following a well-establish convention.
 *
 * This interpreter works in a similar manner as a processor.
 */
class StackInterpreter extends Interpreter with BugReporter {
  import Language._

  type Val = Int
  type Loc = Int

  /**
   * Env of the interpreter. Keep track of the location
   * in memory of each variable defined.
   */
  class Env {
    def apply(name: String): Loc =
      BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")
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
      override def apply(name: String) = vars.get(name) match {
        case Some(loc) => loc
        case None => outer(name)
      }
  }

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and store it at 'sp'
   *
   * TODO: Implement the appropriate code as defined in the handout.
   */
  def evalUn(op: String)(sp: Loc) = op match {
    case "-" => memory(sp) = -memory(sp)
    case "+" => memory(sp) = memory(sp)
    case _ => BUG(s"Unary operator $op undefined")
  }

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and 'sp1', and store it at 'sp'
   *
   * NOTE: sp op sp1
   */
  def evalBin(op: String)(sp: Loc, sp1: Loc) = op match {
    case "+" => memory(sp) += memory(sp1)
    case "-" => memory(sp) -= memory(sp1)
    case "*" => memory(sp) *= memory(sp1)
    case "/" => memory(sp) /= memory(sp1)
  }

  /*
   * Compute the result of the operator 'op' on the
   * value stored at 'sp' and 'sp1', and store it in the
   * variable 'flag'.
   *
   * NOTE: sp op sp1
   *
   * TODO: Implement the appropriate code as defined in the handout.
   */
  def evalCond(op: String)(sp: Loc, sp1: Loc) = {
    flag = op match {
      case "!=" => memory(sp) != memory(sp1)
      case "<=" => memory(sp) <= memory(sp1)
      case ">=" => memory(sp) >= memory(sp1)
      case "<"  => memory(sp) < memory(sp1)
      case ">"  => memory(sp) > memory(sp1)
      case "==" => memory(sp) == memory(sp1)
      case _ => BUG(s"Binary operator $op undefined")
    }
  }

  // Memory and flag used by the interpreter
  val memory = new Array[Val](1000)
  var flag: Boolean = true

  /*
   * Evaluate the value of the AST 'exp' within
   * an empty environment and return the value.
   */
  def run(exp: Exp): Val = {
    eval(exp, 0)(LocationEnv())
    memory(0)
  }

  /*
   * Evaluate the value of the AST 'exp' within
   * the environment 'env 'and store the result
   * at 'sp'.
   *
   * NOTE: Cond stores its result in the 'flag'
   * variable.
   *
   * TODO: Remove all ???s and implement the
   * appropriate code as defined in the handout.
   */
  def eval(exp: Exp, sp: Loc)(env: LocationEnv): Unit = exp match {
    case Lit(x) =>
      memory(sp) = x
    case Unary(op, v) =>
      eval(v, sp)(env)
      evalUn(op)(sp)
    case Prim(op, lop, rop) =>
      eval(lop, sp)(env)
      eval(rop, sp+1)(env)
      evalBin(op)(sp, sp+1)
    case Let(x, a, b) => 
      eval(a, sp)(env)
      eval(b, sp+1)(env.withVal(x, sp))
      memory(sp) = memory(sp+1)
    case Ref(x) => 
      memory(sp) = memory(env.apply(x))
    case Cond(op, l, r) =>
      eval(l, sp)(env)
      eval(r, sp+1)(env)
      evalCond(op)(sp, sp+1)
    case If(cond, tBranch, eBranch) =>
      eval(cond, sp)(env)
      if (flag)
        eval(tBranch, sp)(env)
      else
        eval(eBranch, sp)(env)
    case VarDec(x, rhs, body) =>
      eval(rhs, sp)(env)
      eval(body, sp+1)(env.withVal(x, sp))
      memory(sp) = memory(sp+1)
    case VarAssign(x, rhs) =>
      eval(rhs, sp)(env)
      memory(env.apply(x)) = memory(sp)
    case While(cond, lbody, body) =>
      eval(cond, sp)(env)
      while (flag) {
        eval(lbody, sp)(env)
        eval(cond, sp)(env)
      }
      eval(body, sp)(env)
  }
}
