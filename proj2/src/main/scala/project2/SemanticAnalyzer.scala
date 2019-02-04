package project2

class SemanticAnalyzer(parser: Parser) extends Reporter {
  import Language._

  /*
   * Define an empty state for the Semantic Analyser.
   *
   * NOTE:
   *   val env = new Env
   *
   *   env("hello") is equivalent to env.apply("hello")
   */
  class Env {
    def apply(name: String) = false
    def isVar(name: String) = false
  }

  /*
   * Env that keeps track of variables defined.
   * The map stores true if the variable is mutable,
   * false otherwise.
   */
  case class VarEnv(
    vars: Map[String,Boolean] = Map.empty,
    outer: Env = new Env) extends Env {

    /*
     * Return true if the variable is already defined
     * in this scope
     */
    def isDefined(name: String) = vars.contains(name)


    /*
     * Make a copy of this object and add a mutable variable 'name'
     */
    def withVar(name: String): VarEnv = {
      copy(vars = vars + (name -> true))
    }

    /*
     * Make a copy of this object and add an immutable variable 'name'
     */
    def withVal(name: String): VarEnv = {
      copy(vars = vars + (name -> false))
    }

    /*
     * Return true if 'name' is a mutable variable defined in this scope
     * or in the outer scope.
     */
    override def isVar(name: String) = vars.get(name) match {
      case None => outer.isVar(name)
      case Some(mut) => mut
    }

    /*
     * Return true if 'name' is a variable defined in this scope
     * or in the outer scope.
     */
    override def apply(name: String): Boolean = isDefined(name) || outer(name)
  }

  // Error reporting.
  var numError = 0
  def error(msg: String, pos: Position): Unit = {
    numError += 1
    parser.error(msg, pos)
  }

  // Warning reporting.
  var numWarning = 0
  def warn(msg: String, pos: Position): Unit = {
    numWarning += 1
    parser.warn(msg, pos)
  }

  /**
   * Run the Semantic Analyzer on the given AST.
   * Print out the number of warnings and errors
   * found, if any.
   * Return the number of warnings and errors
   */
  def run(exp: Exp) = {
    numError = 0
    numWarning = 0
    analyze(exp)(VarEnv())
    if (numWarning > 0)
      System.err.println(s"""$numWarning warning${if (numWarning != 1) "s" else ""} found""")
    if (numError > 0)
      System.err.println(s"""$numError error${if (numError != 1) "s" else ""} found""")
    else
      System.out.println("Correct semantic")

    (numWarning, numError)
  }

  // List of valid infix operators
  val isOperator   = Set("+","-","*","/")

  // List of valid unary operators
  val isUnOperator   = Set("+","-")

  // List of valid boolean operators
  val isBOperator = Set("==", "!=", "<=", ">=", "<", ">")

  /*
   * Analyze 'exp' with the environment 'env'
   *
   * TODO: Remove the () and add the correct implementation.
   * The code must follow the rules listed in the handout.
   * Use the error and warning methods provided.
   */
  def analyze(exp: Exp)(env: VarEnv): Unit = exp match {
    case Lit(x) =>
      () // Correct there is nothing to check here.
    case Unary(op, v) =>
      if (!isUnOperator(op))
        error("undefined unary operator", exp.pos)
      analyze(v)(env)
    case Prim(op, lop, rop) =>
      // TODO - Done
      if (!isOperator(op))
        error("undefined binary operator", exp.pos)
      analyze(lop)(env)
      analyze(rop)(env)
    case Let(x, a, b) =>
      // TODO - Done
      if(env.isDefined(x))
        warn("reuse of variable name", exp.pos)
      analyze(a)(env)
      analyze(b)(env.withVal(x))
    case Ref(x) =>
      // TODO - Done
      if(!env.isDefined(x))
        error("undefined identifier", exp.pos)
    case Cond(op, l, r) =>
      // TODO - Done
      if (!isBOperator(op))
        error("undefined boolean operator", exp.pos)
      analyze(l)(env)
      analyze(r)(env)
    case If(cond, tBranch, eBranch) =>
      analyze(cond)(env)
      analyze(tBranch)(env)
      analyze(eBranch)(env)
    case VarDec(x, rhs, body) =>
      // TODO - Done
      if(env.isDefined(x))
        warn("reuse of variable name", exp.pos) 
      analyze(rhs)(env)
      analyze(body)(env.withVar(x))
    case VarAssign(x, rhs) =>
      // TODO - Done
      if(!env.isDefined(x))
        error("undefined identifier", exp.pos)
      if(!env.isVar(x))
        error("reassignment to val", exp.pos)
      analyze(rhs)(env)
    case While(cond, lBody, body) =>
      // TODO - Done
      analyze(cond)(env)
      analyze(lBody)(env)
      analyze(body)(env)
    case _ => abort(s"unknown AST node $exp")
  }
}
