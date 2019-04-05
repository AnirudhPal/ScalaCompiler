package miniscala

import scala.collection.mutable.{ Map => MutableMap }

abstract class CPSOptimizer[T <: CPSTreeModule { type Name = Symbol }]
  (val treeModule: T) {
  import treeModule._

  def apply(tree: Tree): Tree = {
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = (size(simplifiedTree) * 1.5).toInt
    fixedPoint(simplifiedTree, 8) { t => inline(t, maxSize) }
  }

  /* Counts how many times a symbol is encountered as an applied function,
   * and how many as a value
   */
  private case class Count(applied: Int = 0, asValue: Int = 0)

  /* Local state of the optimization
   * Note: To update the state, use the with* methods
   */
  private case class State(
    /* How many times a symbol is encountered in the Tree. Note: The
     * census for the whole program gets calculated once in the
     * beginning, and passed to the initial state.
     */
    census: Map[Name, Count],
    // Name substitution that needs to be applied to the current tree
    subst: Substitution[Name] = Substitution.empty,
    // Names that have a constant value
    lEnv: Map[Name, Literal] = Map.empty,
    // The inverse of lEnv
    lInvEnv: Map[Literal, Name] = Map.empty,
    // A known block mapped to its tag and length
    bEnv: Map[Name, (Literal, Name)] = Map.empty,
    // ((p, args) -> n2) is included in eInvEnv iff n2 == p(args)
    // Note: useful for common-subexpression elimination
    eInvEnv: Map[(ValuePrimitive, Seq[Name]), Name] = Map.empty,
    // Continuations that will be inlined
    cEnv: Map[Name, CntDef] = Map.empty,
    // Functions that will be inlined
    fEnv: Map[Name, FunDef] = Map.empty) {

    // Checks whether a symbol is dead in the current state
    def dead(s: Name): Boolean =
      census get s map (_ == Count(applied = 0, asValue = 0)) getOrElse true
    // Checks whether a symbols is applied exactly once as a function
    // in the current State, and never used as a value
    def appliedOnce(s: Name): Boolean =
      census get s map (_ == Count(applied = 1, asValue = 0)) getOrElse false

    // Addas a substitution to the state
    def withSubst(from: Name, to: Name): State =
      copy(subst = subst + (from -> to))
    // Adds a Seq of substitutions to the state
    def withSubst(from: Seq[Name], to: Seq[Name]): State =
      copy(subst = subst ++ (from zip to))

    // Adds a constant to the State
    def withLit(name: Name, value: Literal) =
      copy(lEnv = lEnv + (name -> value), lInvEnv = lInvEnv + (value -> name))
    // Adds a block to the state
    def withBlock(name: Name, tag: Literal, size: Name) =
      copy(bEnv = bEnv + (name -> (tag, size)))
    // Adds a primitive assignment to the state
    def withExp(name: Name, prim: ValuePrimitive, args: Seq[Name]) =
      copy(eInvEnv = eInvEnv + ((prim, args) -> name))
    // Adds an inlinable continuation to the state
    def withCnt(cnt: CntDef) =
      copy(cEnv = cEnv + (cnt.name -> cnt))
    // Adds a Seq of inlinable continuations to the state
    def withCnts(cnts: Seq[CntDef]) =
      (this /: cnts) (_.withCnt(_))
    // Adds an inlinable function to the state
    def withFun(fun: FunDef) =
      copy(fEnv = fEnv + (fun.name -> fun))
    // Adds a Seq of inlinable functions to the state
    def withFuns(funs: Seq[FunDef]) =
      (this /: funs) (_.withFun(_))
    /*
     * The same state, with emply inverse environments.
     * Use this when entering a new FunDef, because assigned Name's may
     * come out of scope during hoisting.
     */
    def withEmptyInvEnvs =
      copy(lInvEnv = Map.empty, eInvEnv = Map.empty)
  }

  // Shrinking optimizations

  private def shrink(tree: Tree): Tree = {
    def shrinkT(tree: Tree)(implicit s: State): Tree = tree match {

      // LetL
      case LetL(name, value, body) =>
	// DCE
	if(s.dead(name)) {
        	shrinkT(body)(s)
	}
      	// CSE
      	else if(s.lInvEnv.contains(value)) {
        	shrinkT(body.subst(Substitution(name, s.lInvEnv(value))))(s)
	}
  	// ->
	else {
        	LetL(name, value, shrinkT(body)(s.withLit(name, value)))
	}

      // LetP
      case LetP(name, op, args, body) =>
	// DCE
	if(s.dead(name) && !impure(op)) {
        	shrinkT(body)(s)
	}
	// CSE
	else if(!impure(op) && !unstable(op) && s.eInvEnv.contains((op, args))) {
        	shrinkT(body.subst(Substitution(name, s.eInvEnv((op, args)))))(s)
	}
	// CF
      	else if(args.length == 2 && args.forall(a => s.lEnv.contains(a))) {
        	val lits = args map {a => s.lEnv(a)}
        	LetL(name, vEvaluator(op, lits), shrinkT(body)(s.withLit(name, vEvaluator(op, lits))))
	}
	// ID
	else if(args.length == 1 && op == identity) {
        	shrinkT(body.subst(Substitution(name, args(0))))(s)
	}
	// Neut/Abs
	else if(args.length == 2) {
		// LN
		if(s.lEnv.contains(args(0)) && leftNeutral.contains((s.lEnv(args(0)), op))) { 
        		shrinkT(body.subst(Substitution(name, args(1))))(s)
		}
		// RN
      		else if(s.lEnv.contains(args(1)) && rightNeutral.contains((op, s.lEnv(args(1))))) { 
        		shrinkT(body.subst(Substitution(name, args(0))))(s)
		}
		// LA
		else if(s.lEnv.contains(args(0)) && leftAbsorbing.contains((s.lEnv(args(0)), op))) { 
        		shrinkT(body.subst(Substitution(name, args(0))))(s)
		}
		// RA
		else if(s.lEnv.contains(args(1)) && rightAbsorbing.contains((op, s.lEnv(args(1))))) { 
        		shrinkT(body.subst(Substitution(name, args(1))))(s)
		}
      		// SAN
		else if(args(0) == args(1)) { 
        		LetL(name, sameArgReduce(op), shrinkT(body)(s))
		}
		else {
			LetP(name, op, args, shrinkT(body)(s.withExp(name, op, args)))
		}
	}
	// -> 
      	else {
        	LetP(name, op, args, shrinkT(body)(s.withExp(name, op, args)))
	}
      
      // IF
      case If(cond, args, ct, cf) =>
	// CF
	if(args.length == 2 && s.lEnv.contains(args(0)) && s.lEnv.contains(args(1)) && (s.lEnv(args(0)) == s.lEnv(args(1)))) { 
		if(sameArgReduceC(cond)) {         	
			AppC(ct, Seq())
		}
		else {
        		AppC(cf, Seq())
		}
	}
	// CF
      	else if(args.forall(a => s.lEnv.contains(a))) {
        	val lits = args map {(arg: Name) => s.lEnv(arg)}
        	if(cEvaluator(cond, lits)) { 
			AppC(ct, Seq())
		} 
		else {
			AppC(cf, Seq())
		}
	}
      	else {
		tree
	}
      
      // Rest
      case _ => tree
      
    }

    shrinkT(tree)(State(census(tree)))
  }

  // (Non-shrinking) inlining

  private def inline(tree: Tree, maxSize: Int): Tree = {

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = Stream.iterate((0, tree), fibonacci.length) { case (i, tree) =>
      val funLimit = fibonacci(i)
      val cntLimit = i

      def inlineT(tree: Tree)(implicit s: State): Tree = tree match {
        case _ =>
          // TODO
          tree
      }

      (i + 1, fixedPoint(inlineT(tree)(State(census(tree))))(shrink))
    }

    trees.takeWhile{ case (_, tree) => size(tree) <= maxSize }.last._2
  }

  // Census computation
  private def census(tree: Tree): Map[Name, Count] = {
    val census = MutableMap[Name, Count]()
    val rhs = MutableMap[Name, Tree]()

    def incAppUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(applied = currCount.applied + 1)
      rhs remove symbol foreach addToCensus
    }

    def incValUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(asValue = currCount.asValue + 1)
      rhs remove symbol foreach addToCensus
    }

    def addToCensus(tree: Tree): Unit = (tree: @unchecked) match {
      case LetL(_, _, body) =>
        addToCensus(body)
      case LetP(_, _, args, body) =>
        args foreach incValUse; addToCensus(body)
      case LetC(cnts, body) =>
        rhs ++= (cnts map { c => (c.name, c.body) }); addToCensus(body)
      case LetF(funs, body) =>
        rhs ++= (funs map { f => (f.name, f.body) }); addToCensus(body)
      case AppC(cnt, args) =>
        incAppUse(cnt); args foreach incValUse
      case AppF(fun, retC, args) =>
        incAppUse(fun); incValUse(retC); args foreach incValUse
      case If(_, args, thenC, elseC) =>
        args foreach incValUse; incValUse(thenC); incValUse(elseC)
      case Halt(arg) =>
        incValUse(arg)
    }

    addToCensus(tree)
    census.toMap
  }

  private def sameLen(formalArgs: Seq[Name], actualArgs: Seq[Name]): Boolean =
    formalArgs.length == actualArgs.length

  private def size(tree: Tree): Int = (tree: @unchecked) match {
    case LetL(_, _, body) => size(body) + 1
    case LetP(_, _, _, body) => size(body) + 1
    case LetC(cs, body) => (cs map { c => size(c.body) }).sum + size(body)
    case LetF(fs, body) => (fs map { f => size(f.body) }).sum + size(body)
    case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) => 1
  }


  // Returns whether a ValuePrimitive has side-effects
  protected val impure: ValuePrimitive => Boolean
  // Returns whether different applications of a ValuePrimivite on the
  // same arguments may yield different results
  protected val unstable: ValuePrimitive => Boolean
  // Extracts the tag from a block allocation primitive
  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal]
  // Returns true for the block tag primitive
  protected val blockTag: ValuePrimitive
  // Returns true for the block length primitive
  protected val blockLength: ValuePrimitive
  // Returns true for the identity primitive
  protected val identity: ValuePrimitive

  // ValuePrimitives with their left-neutral elements
  protected val leftNeutral: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-neutral elements
  protected val rightNeutral: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with their left-absorbing elements
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-absorbing elements
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with the value equal arguments reduce to
  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal]
  // TestPrimitives with the (boolean) value equal arguments reduce to
  protected val sameArgReduceC: TestPrimitive => Boolean
  // An evaluator for ValuePrimitives
  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal]
  // An evaluator for TestPrimitives
  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean]
}

object CPSOptimizerHigh extends CPSOptimizer(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => SymbolicCPSTreeModule.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(MiniScalaBlockSet, MiniScalaByteRead, MiniScalaByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    // TODO - Direct Translation from Low
		case MiniScalaBlockAlloc(_) | MiniScalaBlockGet | MiniScalaByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case MiniScalaBlockAlloc(tag) => IntLit(tag)
  }
  protected val blockTag: ValuePrimitive = MiniScalaBlockTag
  protected val blockLength: ValuePrimitive = MiniScalaBlockLength

  protected val identity: ValuePrimitive = MiniScalaId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
		// TODO - Direct Translation from Low
    Set((IntLit(0), MiniScalaIntAdd), (IntLit(1), MiniScalaIntMul), 
				(IntLit(~0), MiniScalaIntBitwiseAnd), (IntLit(0), MiniScalaIntBitwiseOr), 
				(IntLit(0), MiniScalaIntBitwiseXOr))
  
	protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
		// TODO - Direct Translation from Low
    Set((MiniScalaIntAdd, IntLit(0)), (MiniScalaIntSub, IntLit(0)), 
				(MiniScalaIntMul, IntLit(1)), (MiniScalaIntDiv, IntLit(1)),
        (MiniScalaIntArithShiftLeft, IntLit(0)), (MiniScalaIntArithShiftRight, IntLit(0)),
        (MiniScalaIntBitwiseAnd, IntLit(~0)), (MiniScalaIntBitwiseOr, IntLit(0)), 
				(MiniScalaIntBitwiseXOr, IntLit(0)))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
		// TODO - Direct Translation from Low
    Set((IntLit(0), MiniScalaIntMul), (IntLit(0), MiniScalaIntBitwiseAnd), 
				(IntLit(~0), MiniScalaIntBitwiseOr))

  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
		// TODO - Direct Translation from Low
    Set((MiniScalaIntMul, IntLit(0)), (MiniScalaIntBitwiseAnd, IntLit(0)), 
				(MiniScalaIntBitwiseOr, IntLit(~0)))

  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal] =
		// TODO - Direct Translation from Low
    Map(MiniScalaIntSub -> IntLit(0), MiniScalaIntDiv -> IntLit(1), 
				MiniScalaIntMod -> IntLit(0), MiniScalaIntBitwiseXOr -> IntLit(0))

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case MiniScalaIntLe | MiniScalaIntGe | MiniScalaEq => true
    case MiniScalaIntLt | MiniScalaIntGt | MiniScalaNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
		// TODO - Direct Translation from Low
    case (MiniScalaIntAdd, Seq(IntLit(x), IntLit(y))) => IntLit(x + y)
		case (MiniScalaIntSub, Seq(IntLit(x), IntLit(y))) => IntLit(x - y)
 		case (MiniScalaIntMul, Seq(IntLit(x), IntLit(y))) => IntLit(x * y)
		case (MiniScalaIntDiv, Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(Math.floorDiv(x, y))
		case (MiniScalaIntMod, Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(Math.floorMod(x, y))
		case (MiniScalaIntArithShiftLeft, Seq(IntLit(x), IntLit(y))) => IntLit(x << y)
		case (MiniScalaIntArithShiftRight, Seq(IntLit(x), IntLit(y))) => IntLit(x >> y)
		case (MiniScalaIntBitwiseAnd, Seq(IntLit(x), IntLit(y))) => IntLit(x & y)
		case (MiniScalaIntBitwiseOr, Seq(IntLit(x), IntLit(y))) => IntLit(x | y)
		case (MiniScalaIntBitwiseXOr, Seq(IntLit(x), IntLit(y))) => IntLit(x ^ y)
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {
		// TODO - Translation with Types from Low
    case (MiniScalaIntP, Seq(IntLit(_))) => true
		case (MiniScalaIntP, _) => false
		case (MiniScalaBoolP, Seq(BooleanLit(_))) => true
		case (MiniScalaBoolP, _) => false
		case (MiniScalaCharP, Seq(CharLit(_))) => true
		case (MiniScalaCharP, _) => false
		case (MiniScalaUnitP, Seq(UnitLit)) => true
		case (MiniScalaUnitP, _) => false
		case (MiniScalaEq, Seq(BooleanLit(x), BooleanLit(y))) => x == y
	  case (MiniScalaNe, Seq(BooleanLit(x), BooleanLit(y))) => x != y
		case (MiniScalaEq, Seq(CharLit(x), CharLit(y))) => x == y
	  case (MiniScalaNe, Seq(CharLit(x), CharLit(y))) => x != y
		case (MiniScalaEq, Seq(IntLit(x), IntLit(y))) => x == y
	  case (MiniScalaNe, Seq(IntLit(x), IntLit(y))) => x != y	
		case (MiniScalaIntLt, Seq(IntLit(x), IntLit(y))) => x < y
	  case (MiniScalaIntLe, Seq(IntLit(x), IntLit(y))) => x <= y
		case (MiniScalaIntGe, Seq(IntLit(x), IntLit(y))) => x >= y
	  case (MiniScalaIntGt, Seq(IntLit(x), IntLit(y))) => x > y
  }
}

object CPSOptimizerLow extends CPSOptimizer(SymbolicCPSTreeModuleLow)
    with (SymbolicCPSTreeModuleLow.Tree => SymbolicCPSTreeModuleLow.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(CPSBlockSet, CPSByteRead, CPSByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    case CPSBlockAlloc(_) | CPSBlockGet | CPSByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case CPSBlockAlloc(tag) => tag
  }
  protected val blockTag: ValuePrimitive = CPSBlockTag
  protected val blockLength: ValuePrimitive = CPSBlockLength

  protected val identity: ValuePrimitive = CPSId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSAdd), (1, CPSMul), (~0, CPSAnd), (0, CPSOr), (0, CPSXOr))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((CPSAdd, 0), (CPSSub, 0), (CPSMul, 1), (CPSDiv, 1),
        (CPSArithShiftL, 0), (CPSArithShiftR, 0),
        (CPSAnd, ~0), (CPSOr, 0), (CPSXOr, 0))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSMul), (0, CPSAnd), (~0, CPSOr))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((CPSMul, 0), (CPSAnd, 0), (CPSOr, ~0))

  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(CPSSub -> 0, CPSDiv -> 1, CPSMod -> 0, CPSXOr -> 0)

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case CPSLe | CPSGe | CPSEq => true
    case CPSLt | CPSGt | CPSNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (CPSAdd, Seq(x, y)) => x + y
    case (CPSSub, Seq(x, y)) => x - y
    case (CPSMul, Seq(x, y)) => x * y
    case (CPSDiv, Seq(x, y)) if (y != 0) => Math.floorDiv(x, y)
    case (CPSMod, Seq(x, y)) if (y != 0) => Math.floorMod(x, y)

    case (CPSArithShiftL, Seq(x, y)) => x << y
    case (CPSArithShiftR, Seq(x, y)) => x >> y
    case (CPSAnd, Seq(x, y)) => x & y
    case (CPSOr, Seq(x, y)) => x | y
    case (CPSXOr, Seq(x, y)) => x ^ y
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {

    case (CPSLt, Seq(x, y)) => x < y
    case (CPSLe, Seq(x, y)) => x <= y
    case (CPSEq, Seq(x, y)) => x == y
    case (CPSNe, Seq(x, y)) => x != y
    case (CPSGe, Seq(x, y)) => x >= y
    case (CPSGt, Seq(x, y)) => x > y
  }
}
