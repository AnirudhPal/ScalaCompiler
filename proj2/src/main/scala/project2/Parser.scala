package project2

// Class used to carry position information within the source code
case class Position(gapLine: Int, gapCol: Int, startLine: Int, startCol: Int, endLine: Int, endCol: Int)

object Tokens {

  abstract class Token {
    var pos: Position = _
  }
  case object EOF extends Token
  case class Number(x: Int) extends Token
  case class Ident(x: String) extends Token
  case class Keyword(x: String) extends Token
  case class Delim(x: Char) extends Token
}


// Scanner
class Scanner(in: Reader[Char]) extends Reader[Tokens.Token] with Reporter {
  import Tokens._

  // Position handling
  def pos = in.pos
  def input = in.input

  // Current line in the file
  var line = 0

  // lineStarts(i) contains the offset of the i th line within the file
  val lineStarts = scala.collection.mutable.ArrayBuffer(0)

  // Current column in the file
  def column = pos - lineStarts(line)

  // Extract the i th line of code.
  def getLine(i: Int) = {
    val start = lineStarts(i)
    val end = input.indexOf('\n', start)

    if (end < 0)
      input.substring(start)
    else
      input.substring(start, end)
  }

  // Information for the current Position
  var gapLine = 0;
  var gapCol = 0;
  var startLine = 0;
  var startCol = 0;
  var endLine = 0;
  var endCol = 0;

  override def abort(msg: String) = {
    abort(msg, showSource(getCurrentPos()))
  }

  /*
   * Show the line of code and highlight the token at position p
   */
  def showSource(p: Position) = {
    val width = if (p.endLine == p.startLine) (p.endCol - p.startCol) else 0

    val header = s"${p.startLine + 1}:${p.startCol + 1}: "
    val line1 = getLine(p.startLine)
    val line2 = " "*(p.startCol+header.length) + "^"*(width max 1)
    header + line1 + '\n' + line2
  }

  def isAlpha(c: Char) =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  def isDigit(c: Char) = '0' <= c && c <= '9'

  def isAlphaNum(c: Char) = isAlpha(c) || isDigit(c)

  def isCommentStart(c1: Char, c2: Char) = c1 == '/' && c2 == '/'

  val isWhiteSpace = Set(' ','\t','\n','\r')

  // Boolean operators start with one of the following characters
  val isBOperator  = Set('<', '>', '!', '=')

  //  Operators start with one of the following characters
  val isOperator   = Set('+','-','*','/') ++ isBOperator

  // List of delimiters
  // TODO: Update this as delimiters are added to our language
  val isDelim      = Set('(',')',';','{','}','=')

  // List of keywords
  // TODO: Update this as keywords are added to our language
  val isKeyword    = Set[String]("val","if","else","var","while")

  /*
   * Extract a name from the stream
   */
  def getName() = {
    val buf = new StringBuilder
    while (in.hasNext(isAlphaNum)) {
      buf += in.next()
    }
    val s = buf.toString
    if (isKeyword(s)) Keyword(s) else Ident(s)
  }

  /*
   * Extract an operator from the stream
   */
  def getOperator() = {
    val buf = new StringBuilder
    do {
      buf += in.next()
    } while (in.hasNext(isOperator))
    val s = buf.toString
    // "=" is a delimiter, "=>" is a keyword, "==","=+", etc are operators
    if (s == "=") Delim('=')
    else if (isKeyword(s)) Keyword(s) else Ident(s)
  }

  /*
   * Extract a number from the stream and return it.
   * Raise an error if there is overflow.
   *
   * NOTE: An integer can be between 0 and (2 to the power 31) minus 1
   * TODO: implement the method
   */
  def getNum() = {
    val buf = new StringBuilder
    while (in.hasNext(isDigit)) {
      buf += in.next()
    }
    try {
      val n = buf.toInt
      Number(n)
    }
    catch {
      case _: Throwable => abort(s"int overflow")
    } 
  }

  /*
   * Extract a raw token from the stream.
   * i.e. without position information.
   */
  def getRawToken(): Token = {
    if (in.hasNext(isAlpha)) {
      getName()
    } else if (in.hasNext(isOperator)) {
      getOperator()
    } else if (in.hasNext(isDigit)) {
      getNum()
    } else if (in.hasNext(isDelim)) {
      Delim(in.next())
    } else if (!in.hasNext) {
      EOF
    } else {
      abort(s"unexpected character")
    }
  }

  /*
   * Skip white space and comments. Stop at the next token.
   */
  def skipWhiteSpace() = {
    while (in.hasNext(isWhiteSpace) || in.hasNext2(isCommentStart)) {

      // If it is a comment, consume the full line
      if (in.peek == '/') {
        in.next()
        while (in.peek != '\n') in.next()

      }

      // Update file statistics if new line
      if (in.peek == '\n') {
        lineStarts += pos + 1
        line += 1
      }
      in.next()
    }
  }

  def getCurrentPos() = {
    endLine = line; endCol = column
    Position(gapLine,gapCol,startLine,startCol,endLine,endCol)
  }

  /*
   * Extract a token and set position information
   */
  def getToken(): Token = {
    gapLine = line; gapCol = column
    skipWhiteSpace()
    startLine = line; startCol = column
    val tok = getRawToken()
    tok.pos = getCurrentPos()

    tok
  }

  var peek  = getToken()
  var peek1 = getToken()
  def hasNext: Boolean = peek != EOF
  def hasNext(f: Token => Boolean) = f(peek)
  def hasNext2(f: (Token, Token) => Boolean) = f(peek, peek1)
  def next() = {
    val res = peek
    peek = peek1
    peek1 = getToken()
    res
  }
}

class Parser(in: Scanner) extends Reporter {
  import Tokens._

  /*
   * Overloaded methods that show the source code
   * and highlight the current token when reporting
   * an error.
   */
  override def expected(msg: String) = {
    expected(msg, in.showSource(in.peek.pos))
  }

  override def abort(msg: String) = {
    abort(msg, in.showSource(in.peek.pos))
  }

  def error(msg: String, pos: Position): Unit =
    error(msg, in.showSource(pos))

  def warn(msg: String, pos: Position): Unit =
    warn(msg, in.showSource(pos))

  def accept(c: Char) = {
    if (in.hasNext(_ == Delim(c))) in.next()
    else expected(s"'$c'")
  }

  def accept(s: String) = {
    if (in.hasNext(_ == Keyword(s))) in.next()
    else expected(s"'$s'")
  }

  /*
   * Auxilaries functions
   * Test and extract data
   */
  def isName(x: Token) = x match {
    case Ident(x) => true
    case _ => false
  }

  def getName(): (String, Position) = {
    if (!in.hasNext(isName)) expected("Name")
    val pos = in.peek.pos
    val Ident(x) = in.next()
    (x, pos)
  }

  def isNum(x: Token) = x match {
    case Number(x) => true
    case _ => false
  }

  def getNum(): (Int, Position) = {
    if (!in.hasNext(isNum)) expected("Number")
    val pos = in.peek.pos
    val Number(x) = in.next()
    (x, pos)
  }

  def getOperator(): (String, Position) = {
    if (!in.hasNext(isName)) expected("Operator")
    val pos = in.peek.pos
    val Ident(x) = in.next()
    (x, pos)
  }

  /*
   * Test if the following token is an infix
   * operator with highest precedence
   */
  def isInfixOp(min: Int)(x: Token) = x match {
    case Ident(x) => prec(x) >= min
    case _ => false
  }

  /*
   * Test if the following token is an operator.
   */
  def isOperator(x: Token) = x match {
    case Ident(x) => in.isOperator(x.charAt(0))
    case _ => false
  }

  /*
   * Define precedence of operator.
   * Negative precedence means that the operator can
   * not be used as an infix operator within a simple expression.
   */
  def prec(a: String) = a match { // higher bind tighter
    case "+" | "-" => 1
    case "*" | "/" => 2
    case _ if in.isBOperator(a.charAt(0)) => -1
    case _ => 0
  }

  def assoc(a: String) = a match {
    case "+" | "-" | "*" | "/"  => 1
    case _    => 1
  }
}


/**
 * Definition of our target language.
 *
 * The different nodes of the AST also keep Position information
 * for error handling during the semantic analysis.
 *
 * TODO: Every time you add an AST node, you must also track the position
 */
object Language {
  abstract class Exp {
    var pos: Position = _
    def withPos(p: Position) = {
      pos = p
      this
    }
  }


  // Arithmetic
  case class Lit(x: Int) extends Exp
  case class Unary(op: String, v: Exp) extends Exp
  case class Prim(op: String, lop: Exp, rop: Exp) extends Exp

  // Immutable variables
  case class Let(x: String, a: Exp, b: Exp) extends Exp
  case class Ref(x: String) extends Exp

  // Branches
  case class Cond(op: String, lop: Exp, rop: Exp) extends Exp
  case class If(cond: Cond, tBranch: Exp, eBranch: Exp) extends Exp

  // Mutable variables
  case class VarDec(x: String, rhs: Exp, body: Exp) extends Exp
  case class VarAssign(x: String, rhs: Exp) extends Exp

  // While loops
  case class While(cond: Cond, lBody: Exp, body: Exp) extends Exp
}

/*
 * In the previous project, we highlighted some of the difficulties of operator precedence. We proposed a solution
 * that works well, but requires some duplication of code. What happens if we add another operator such as '&'?
 * What is the correct parsing for 3 & 1 + 3 * 8 & 2?
 *
 *  1) (3 & 1) + (3 * (8 & 2))   & has higher precedence than + and *
 *  2) (3 & 1) + ((3 * 8) & 2)   & has higher precedence than + but lower than *
 *  3) (3 & (1 + (3 * 8))) & 2   & has lower precedence than + and * and is left associative
 *  4) 3 & ((1 + (3 * 8)) & 2)   & has lower precedence than + and * and is right associative
 *
 * In any case, it seems that we would add a new function to handle this operator. And then we must think
 * about what will happen with '==',
 * '~', '|'  etc.!
 *
 * We are therefore going to implement the algorithm we have seen in class and parse the following grammar. The
 * operator precedence is given through the prec function defined in Parser, and the associativity is given
 * through the assoc function.
 *
 * <op>    ::= ['*' | '/' | '+' | '-']+
 * <atom>  ::= <number>
 *           | '('<exp>')'
 * <uatom> ::= [<op>]<atom>
 * <exp>   ::= <uatom>[<op><uatom>]*
 *
 * We have expanded our grammar to allow many more operators. For example "+++++++++" would be a valid syntax.
 * We don't really need it for now, but it will be useful later. It also allows us to handle generic
 * operators.
 *
 * We also introduce unary operators. These are always attached to the subsequent atom. There is no precedence
 * associated with them.
 *
 * Our grammar may look a little bit nondeterministic now. For example, how do we parse the following code
 * (Lit omitted)?
 *
 * 1*-4     => Prim("*-", 1, 4)
 *          => Prim("*", 1, Unary("-", 4))
 *
 * We are going to enforce the "longest match rule." When extracting the first operator in 1*-4, the longest
 * match that satisfies the grammar is "*-". However, 1* -4 will be parsed as Prim("*", 1, Unary("-", 4)).
 * The longest match rule has already been implemented for you in the Scanner class.
 *
 * The parser only enforces syntax: even if "*-" is not a valid operation, it is valid syntax. The semantic
 * analysis will catch the error.
 *
 * TODO: complete the implementation of the parseExpression method using the algorithm we have seen in class.
 */
class ArithParser(in: Scanner) extends Parser(in) {
  import Language._
  import Tokens._

  def parseCode = {
    val res = parseExpression
    if (in.hasNext)
      expected(s"EOF")
    res
  }

  def parseAtom: Exp = in.peek match {
    case Delim('(') =>
      in.next()
      val res = parseExpression
      accept(')')
      res
    case Number(x) =>
      val (lit, pos) = getNum
      Lit(lit).withPos(pos)
    case _ => expected(s"Atom")
  }

  def parseUAtom: Exp = if (in.hasNext(isOperator)) {
    val (op, pos) = getOperator
    Unary(op, parseAtom).withPos(pos)
  } else {
    parseAtom
  }


  def parseExpression: Exp = parseExpression(0)
  def parseExpression(min: Int): Exp = {
    var res = parseUAtom
    // TODO: complete
    while (in.hasNext(isInfixOp(min))) {
      val (op, pos) = getOperator
      val nextMin = prec(op) + assoc(op)
      res = Prim(op, res, parseExpression(nextMin)).withPos(pos) 
    }
    res
  }
}

/*
 * Now we can introduce immutable variables. An atom can now be
 * a reference to a variable exactly like a number.
 *
 * We do not introduce the variable declaration at the
 * same level as the expressions we have used thus far.
 * Instead, we downgrade the arithmetic expression to a simple expression,
 * as shown in the BNF below.
 *
 * For now we can say that it is a design choice.
 *
 * TODO: implement the case Ident in parseAtom
 *
 * <op>    ::= ['*' | '/' | '+' | '-']+
 * <atom>  ::= <number>
 *           | '('<simp>')'
 *           | <ident>
 * <uatom> ::= [<op>]<atom>
 * <simp>  ::= <uatom>[<op><uatom>]*
 * <exp>   ::= <simp>
 *           | 'val' <ident> '=' <simp>';' <exp>
 */
class LetParser(in: Scanner) extends ArithParser(in) {
  import Language._
  import Tokens._

  override def parseAtom: Exp = in.peek match {
    case Delim('(') =>
      in.next()
      val res = parseSimpleExpression
      accept(')')
      res
    case Number(x) =>
      val (_, pos) = getNum
      Lit(x).withPos(pos)
    case Ident(x) =>
      // TODO: remove ??? and add the implementation for that case
      val (_, pos) = getName
      Ref(x).withPos(pos)
    case _ => abort(s"Illegal start of simple expression")
  }

  def parseSimpleExpression = super.parseExpression

  override def parseExpression = in.peek match {
    case Keyword("val") =>
      in.next()
      val (name, pos) = getName
      accept('=')
      val rhs = parseSimpleExpression
      accept(';')
      val body = parseExpression
      Let(name, rhs, body).withPos(pos)
    case _ => parseSimpleExpression
  }
}

/*
 * We can now add if-else statements to our language. Because
 * we don't yet have Boolean variables, we instead define a list
 * of boolean operators which can be used. Similarly, we define
 * an if-else statement as described in the BNF below:
 *
 * <op>    ::= ['*' | '/' | '+' | '-' | '<' | '>' | '=' | '!']+
 * <bop>   ::= ('<' | '>' | '=' | '!')[<op>]
 * <atom>  ::= <number>
 *           | '('<simp>')'
 *           | <ident>
 *           | '{'<exp>'}'
 * <uatom> ::= [<op>]<atom>
 * <cond>  ::= <simp><bop><simp>
 * <simp>  ::= <uatom>[<op><uatom>]*
 *          | 'if' '('<cond>')' <simp> 'else' <simp>
 * <exp>   ::= <simp>
 *           | 'val' <ident> '=' <simp>';' <exp>
 */
class BranchParser(in: Scanner) extends LetParser(in) {
  import Language._
  import Tokens._

  override def parseAtom: Exp = in.peek match {
    case Delim('{') =>
      val pos = in.next().pos
      val res = parseExpression
      accept('}')
      res
    case _ => super.parseAtom
  }

  def parseCondition: Cond = {
    val l = parseSimpleExpression
    if (in.hasNext(isOperator)) {
      val (op, pos) = getOperator
      val r = parseSimpleExpression
      Cond(op, l, r).withPos(pos).asInstanceOf[Cond]
    } else {
      expected(s"operator")
    }
  }

  // TODO: remove ??? and complete the implementation.
  override def parseSimpleExpression = in.peek match {
    case Keyword("if") =>
      val pos = in.next.pos
      accept('(')
      val bVal = parseCondition
      accept(')')
      val rB = parseSimpleExpression
      accept("else")
      val lB = parseSimpleExpression
      If(bVal, rB, lB).withPos(pos)
    case _ => super.parseSimpleExpression
  }
 
}

/*
 * We can now introduce mutable variables using the 'var' keyword.
 * When parsing statements involving mutable variables, we must
 * look for declarations as well as assignments, as described
 * here:
 *
 * <op>    ::= ['*' | '/' | '+' | '-' | '<' | '>' | '=' | '!']+
 * <bop>   ::= ('<' | '>' | '=' | '!')[<op>]
 * <atom>  ::= <number>
 *           | '('<simp>')'
 *           | <ident>
 *           | '{'<exp>'}'
 * <uatom> ::= [<uop>]<atom>
 * <cond>  ::= <exp> <bop> <exp>
 * <simp>  ::= <uatom>[<op><uatom>]*
 *           | 'if' '('<cond>')' <simp> 'else' <simp>
 *           |  <ident> '=' <simp>
 * <exp>   ::= <simp>
 *           | 'val' <ident> '=' <simp>';' <exp>
 *           | 'var' <ident> '=' <simp>';' <exp>
 */
class VariableParser(in: Scanner) extends BranchParser(in) {
  import Language._
  import Tokens._

  // TODO: remove ??? and complete the implementation.
  override def parseExpression = in.peek match {
    case Keyword("var") =>
      in.next()
      val (name, pos) = getName
      accept('=')
      val rhs = parseSimpleExpression
      accept(';')
      val body = parseExpression
      VarDec(name, rhs, body).withPos(pos)
    case _ => super.parseExpression
  }

  // TODO: remove ??? and complete the implementation.
  /*
  override def parseSimpleExpression = in.peek match {
    case Ident(x) =>
      var pos = in.next.pos
      accept('=')
      val res = parseSimpleExpression
      VarAssign(x, res).withPos(pos)
    case _ => super.parseSimpleExpression
  }
  */
  override def parseSimpleExpression = (in.peek, in.peek1) match {
    case (Ident(x), Delim('=')) =>
      var pos = in.next.pos
      accept('=')
      val res = parseSimpleExpression
      VarAssign(x, res).withPos(pos)
    case _ => super.parseSimpleExpression
  }
}

/*
 * Finally, we must parse while loops. These function
 * similar to if-else statements: they consist of a
 * condition and a loop body.
 *
 * <op>    ::= ['*' | '/' | '+' | '-' | '<' | '>' | '=' | '!']+
 * <bop>   ::= ('<' | '>' | '=' | '!')[<op>]
 * <atom>  ::= <number>
 *           | '('<simp>')'
 *           | <ident>
 *           | '{'<exp>'}'
 * <uatom> ::= [<op>]<atom>
 * <cond>  ::= <exp> <bop> <exp>
 * <simp>  ::= <uatom>[<op><uatom>]*
 *           | 'if' '('<cond>')' <simp> 'else' <simp>
 *           |  <ident> '=' <simp>
 * <exp>   ::= <simp>
 *           | 'val' <ident> '=' <simp>';' <exp>
 *           | 'var' <ident> '=' <simp>';' <exp>
 *           | 'while' '('<cond>')'<simp>';' <exp>
 */
class LoopParser(in: Scanner) extends VariableParser(in) {
  import Language._
  import Tokens._

  // TODO: remove ??? and complete the implementation.
  override def parseExpression = in.peek match {
    case Keyword("while") =>
      val pos = in.next.pos
      accept('(')
      val bVal = parseCondition
      accept(')')
      val iC = parseSimpleExpression
      accept(';')
      val oC = parseExpression
      While(bVal, iC, oC).withPos(pos)
    case _ => super.parseExpression
  }
}
