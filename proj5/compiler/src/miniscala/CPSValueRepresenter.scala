package miniscala

import BitTwiddling.bitsToIntMSBF
import miniscala.{ SymbolicCPSTreeModule => H }
import miniscala.{ SymbolicCPSTreeModuleLow => L }

/**
 * Value-representation phase for the CPS language. Translates a tree
 * with high-level values (blocks, integers, booleans, unit) and
 * corresponding primitives to one with low-level values (blocks
 * and integers only) and corresponding primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CPSValueRepresenter extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree =
    transform(tree)(Map.empty)

  val unitLit = bitsToIntMSBF(0, 0, 1, 0)
  val optimized = false

	private def transform(tree: H.Tree)
                       (implicit worker: Map[Symbol, (Symbol, Seq[Symbol])])
      : L.Tree = tree match {

    // Literals
    case H.LetL(name, IntLit(value), body) =>
      L.LetL(name, (value << 1) | 1, transform(body))
    case H.LetL(name, CharLit(value), body) =>
      L.LetL(name, (value << 3) | bitsToIntMSBF(1, 1, 0), transform(body))

    // TODO: Add missing literals

		// BOOL
		// [[val_l name = value; body]] =
		case H.LetL(name, BooleanLit(value), body) => value match {
			// val_l name = (T/F << 4) | 1010;
			// [[body]]
			case true =>
				L.LetL(name, bitsToIntMSBF(1,1,0,1,0), transform(body))
			case false =>
				L.LetL(name, bitsToIntMSBF(0,1,0,1,0), transform(body))
		}

		// UNIT
		// [[val_l name = value; body]] =
		case H.LetL(name, UnitLit,body) =>
			// val_l name = 0010
			L.LetL(name, unitLit,
				// [[body]]
				transform(body))

    // *************** Primitives ***********************
    // Make sure you implement all possible primitives
    // (defined in MiniScalaPrimitives.scala)
    //
    // Integer primitives
    case H.LetP(name, MiniScalaIntAdd, args, body) =>
      tempLetP(CPSAdd, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSSub, Seq(r, c1), transform(body)) } }

    // TODO: Add missing integer primitives

		// SUB -
		// [[val_p name = arg1 - arg2; body]] =
		case H.LetP(name, MiniScalaIntSub, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = args1 - args2;
				tempLetP(CPSSub, args) { t1 =>
					// val_p name = t1 + c1;
					L.LetP(name, CPSAdd, Seq(t1, c1),
						// [[body]]
						transform(body))
				}
			}

		// MUL *
		// [[val_p name = arg1 * arg2; body]] =
		case H.LetP(name, MiniScalaIntMul, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 - c1;
				tempLetP(CPSSub, Seq(args(0), c1)) { t1 =>
					// val_p t2 = arg2 >> c1;
					tempLetP(CPSArithShiftR, Seq(args(1), c1)) { t2 =>
						// val_p t3 = t1 * t2;
						tempLetP(CPSMul, Seq(t1, t2)) { t3 =>
							// val_p name = t3 + c1;
							L.LetP(name, CPSAdd, Seq(t3, c1),
								// [[body]]
								transform(body))
						}
					}
				}
			}

		// DIV /
		// [[val_p name = arg1 / arg2; body]] =
		case H.LetP(name, MiniScalaIntDiv, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 - c1;
				tempLetP(CPSSub, Seq(args(0), c1)) { t1 =>
					// val_p t2 = arg2 >> c1;
					tempLetP(CPSArithShiftR, Seq(args(1), c1)) { t2 =>
						// val_p t3 = t1 / t2;
						tempLetP(CPSDiv, Seq(t1, t2)) { t3 =>
							// val_p name = t3 + c1;
							L.LetP(name, CPSAdd, Seq(t3, c1),
								// [[body]]
								transform(body))
						}
					}
				}
			}

		// MOD %
		// [[val_p name = arg1 % arg2; body]] =
		case H.LetP(name, MiniScalaIntMod, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 >> c1;
				tempLetP(CPSArithShiftR, Seq(args(0), c1)) { t1 =>
					// val_p t2 = arg2 >> c1;
					tempLetP(CPSArithShiftR, Seq(args(1), c1)) { t2 =>
						// val_p t3 = t1 % t2;
						tempLetP(CPSMod, Seq(t1, t2)) { t3 =>
							// val_p t4 = t3 << c1;
							tempLetP(CPSArithShiftL, Seq(t3, c1)) { t4 =>
								// val_p name = t4 | c1;
								L.LetP(name, CPSOr, Seq(t4, c1),
									// [[body]]
									transform(body))
							}
						}
					}
				}
			}

		// LS <<
		// [[val_p name = arg1 << arg2; body]] =
		case H.LetP(name, MiniScalaIntArithShiftLeft, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 - c1;
				tempLetP(CPSSub, Seq(args(0), c1)) { t1 =>
					// val_p t2 = arg2 >> c1;
					tempLetP(CPSArithShiftR, Seq(args(1), c1)) { t2 =>
						// val_p t3 = t1 << t2;
						tempLetP(CPSArithShiftL, Seq(t1, t2)) { t3 =>
							// val_p name = t3 + c1;
							L.LetP(name, CPSAdd, Seq(t3, c1),
								// [[body]]
								transform(body))
						}
					}
				}
			}

		// RS >>
		// [[val_p name = arg1 >> arg2; body]] =
		case H.LetP(name, MiniScalaIntArithShiftRight, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 - c1;
				tempLetP(CPSSub, Seq(args(0), c1)) { t1 =>
					// val_p t2 = arg2 >> c1;
					tempLetP(CPSArithShiftR, Seq(args(1), c1)) { t2 =>
						// val_p t3 = t1 >> t2;
						tempLetP(CPSArithShiftR, Seq(t1, t2)) { t3 =>
							// val_p name = t3 + c1;
							L.LetP(name, CPSAdd, Seq(t3, c1),
								// [[body]]
								transform(body))
						}
					}
				}
			}

		// AND &
		// [[val_p name = arg1 & arg2; body]] =
		case H.LetP(name, MiniScalaIntBitwiseAnd, args, body) =>
			// val_p name = arg1 & arg2;
			L.LetP(name, CPSAnd, args,
				// [[body]]
				transform(body))

		// OR |
		// [[val_p name = arg1 | arg2; body]] =
		case H.LetP(name, MiniScalaIntBitwiseOr, args, body) =>
			// val_p name = arg1 | arg2;
			L.LetP(name, CPSOr, args,
				// [[body]]
				transform(body))

		// XOR ^
		// [[val_p name = arg1 ^ arg2; body]]
		case H.LetP(name, MiniScalaIntBitwiseXOr, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 ^ arg2;
				tempLetP(CPSXOr, args) { t1 =>
					// val_p name = t1 | c1;
					L.LetP(name, CPSOr, Seq(t1, c1),
						// [[body]]
						transform(body))
				}
			}

    // Block primitives
    // TODO: Add block primitives

		// ALLOC
		// [[val_p name = block-alloc-tag(arg1); body]] =
		case H.LetP(name, MiniScalaBlockAlloc(tag), args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 >> c1;
				tempLetP(CPSArithShiftR, Seq(args(0), c1)) { t1 =>
					// val_p name = block-alloc-tag(t1);
					L.LetP(name, CPSBlockAlloc(tag), Seq(t1),
						// [[body]]
						transform(body))
				}
			}

		// TAG
		// [[val_p name = block-tag(arg1); body]] =
		case H.LetP(name, MiniScalaBlockTag, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = block-tag(arg1);
				tempLetP(CPSBlockTag, args) { t1 =>
					// val_p t2 = t1 << c1;
					tempLetP(CPSArithShiftL, Seq(t1, c1)) { t2 =>
						// val_p name = t1 | c1;
						L.LetP(name, CPSOr, Seq(t2, c1),
							// [[body]]
							transform(body))
					}
				}
			}

		// LEN
		// [[val_p name = block-length(arg1); body]] =
		case H.LetP(name, MiniScalaBlockLength, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = block-length(arg1);
				tempLetP(CPSBlockLength, args) { t1 =>
					// val_p t2 = t1 << c1;
					tempLetP(CPSArithShiftL, Seq(t1, c1)) { t2 =>
						// val_p name = t1 | c1;
						L.LetP(name, CPSOr, Seq(t2, c1),
							// [[body]]
							transform(body))
					}
				}
			}


		// GET
		// [[val_p name = block-get(arg1, arg2); body]] =
		case H.LetP(name, MiniScalaBlockGet, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg2 >> c1;
				tempLetP(CPSArithShiftR, Seq(args(1), c1)) { t1 =>
					// val_p name = block-get(arg1, t1);
					L.LetP(name, CPSBlockGet, Seq(args(0), t1),
						// [[body]]
						transform(body))
				}
			}


		// SET
		// [[val_p name = block-set(arg1, arg2, arg3); body]] =
		case H.LetP(name, MiniScalaBlockSet, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg2 >> c1;
				tempLetP(CPSArithShiftR, Seq(args(1), c1)) { t1 =>
					// block-set(arg1, t1, arg3);
					tempLetP(CPSBlockSet, Seq(args(0), t1, args(2))) { dummy =>
						// val_l name = ();
						L.LetL(name, unitLit,
							// [[body]]
							transform(body))
					}
				}
			}

    // Conversion primitives int->char/char->int
    // TODO

		// INT -> CHAR
		// [[val_p name = int->char(arg1); body]] =
		case H.LetP(name, MiniScalaIntToChar, args, body) =>
			// val_l c1 = 2;
			tempLetL(2) { c1 =>
				// val_p t1 = arg1 << c1;
				tempLetP(CPSArithShiftL, Seq(args(0), c1)) { t1 =>
					// val_p name = t1 | c1;
					L.LetP(name, CPSOr, Seq(t1, c1),
						// [[body]]
						transform(body))
				}
			}

		// CHAR -> INT
		// [[val_p name = char->int(arg1); e]] =
		case H.LetP(name, MiniScalaCharToInt, args, body) =>
			// val_l c1 = 2;
			tempLetL(2) { c1 =>
				// val_p name = arg1 >> c1;
				L.LetP(name, CPSArithShiftR, Seq(args(0), c1),
					// [[body]]
					transform(body))
			}

    // IO primitives
    // TODO

		// READ
		// [[val_p name = byte-read(); body]] =
		case H.LetP(name, MiniScalaByteRead, args,body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = byte-read();
				tempLetP(CPSByteRead, Seq()) { t1 =>
					// val_p t2 = t1 << c1;
					tempLetP(CPSArithShiftL, Seq(t1, c1)) { t2 =>
						// val_p name = t2 | c1;
						L.LetP(name, CPSOr, Seq(t2, c1),
							// [[body]]
							transform(body))
					}
				}
			}

		// WRITE
		// [[val_p name = byte-write(arg1); body]] =
		case H.LetP(name, MiniScalaByteWrite, args, body) =>
			// val_l c1 = 1;
			tempLetL(1) { c1 =>
				// val_p t1 = arg1 >> c1;
				tempLetP(CPSArithShiftR, Seq(args(0), c1)) { t1 =>
					// byte-write(t1);
					tempLetP(CPSByteWrite, Seq(t1)) { dummy =>
						// val_p name = ();
						L.LetL(name, unitLit,
							// [[body]]
							transform(body))
					}
				}
			}

    // Other primitives
    // TODO

		// ID
		// [[val_p name = id(args); body]] =
		case H.LetP(name, MiniScalaId, args, body) =>
			// val_p name = id(args);
      L.LetP(name, CPSId, args,
				// [[body]]
				transform(body))

    // Continuations nodes (LetC, AppC)
    // TODO

		// LETC
		// [[def_c c1(args) = {e1}; ...; body]] =
		case H.LetC(cnts, body) =>
			// def_c c1(args) = {[[e1]]}; ...;
			var vrcnts = cnts map { cnt =>
				L.CntDef(cnt.name, cnt.args, transform(cnt.body))
			}
			L.LetC(vrcnts,
				// [[body]]
				transform(body))

		// APPC
		// [[c(args)]] =
		case H.AppC(cnt, args) =>
			// c(args);
			L.AppC(cnt, args)

    // Functions nodes (LetF, AppF)
    // TODO

		// LetF
		case H.LetF(funs, body) =>

      // Environment
      var env = worker

			// List of Value Represeneted Functions
			var vrfuns: Seq[(L.FunDef, L.FunDef, Set[Symbol])] = funs map { fun =>
				// def_f worker_for_f1(c1, args + unit_args) = {[[fbody]][env]};
				// Name of Worker
				val workName = Symbol.fresh(s"worker_for_${fun.name}")
				// Get Free Variables
				val fv = freeVariables(fun)(Map(fun.name -> Set.empty))
				// Get Unit Variables
				val unit_args = fv map { dummy => Symbol.fresh("unitVar")}
				// Create Worker Arguments
				val workArgs = fun.args ++ unit_args
				// Set to Sequence
				val fvSet = fv.toSeq
				// Function Body Environment
        env = env + (fun.name -> (workName ->  fvSet))
				// Create Substitution Table
        var fv_ua = Substitution.empty[Symbol]
        fv_ua = fv_ua ++ (fv zip unit_args).toSeq
				// Create Mutated Body
        val workerBody = fun.body.subst(fv_ua)
				var workerBodyT = transform(workerBody)(env)
        workerBodyT = workerBodyT.subst(fv_ua) // ??
				// Define Worker
				val workDef = L.FunDef(workName, fun.retC, workArgs, workerBodyT)

				// def_f wrapper_for_f1(sc1, env1, sns) = {
				// val_p v1 = block-get(env1, 1); ...
				// worker_for_f1(sc1, sns, vs);
				// Name of Wrapper
        val wrapName = Symbol.fresh(s"wrapper_for_${fun.name}")
				// Wrapper Environment
        val wrapEnv = Symbol.fresh("wrapEnv")
				// Wrapper Continuation
				val wrapCnt = Symbol.fresh(s"wrapCnt") 
				// Get Variable Number
        val vids = List.tabulate(fv.size)(i => i+1)
				// Get Variable Number
        val vs = vids map { dummy => Symbol.fresh(s"v") } 
				// Create Wrapper Arguments
        val sns = fun.args map { dummy => Symbol.fresh(s"sn") }
				// Worker Application
				val workApp = L.AppF(workName, wrapCnt, sns ++ vs)
        // Wrapper Body
        val wrapperBody = wrap(vs zip vids, workApp) {
          case ((n, num), workApp) => tempLetL(num) { c_num =>
            L.LetP(n, CPSBlockGet, Seq(wrapEnv, c_num), workApp)
					}
				}
        // wrapper FunDef
        val wrapDef = L.FunDef(wrapName, wrapCnt, Seq(wrapEnv) ++ sns, wrapperBody)

				// Return Tuple
        (workDef, wrapDef, fv)
      }

      // Reverse Order because of wrap()			
			// [[body]]
			val mainBody = transform(body)(env)

			// val_p t11 = block-set(f1, 0, s1); ...	
			// Get Function Names
      var funNames = funs map { fun => fun.name }
			// Get Wrapper Names
      var wrapNames = vrfuns map { vrfun => vrfun._2.name }
			// Get FVs
      var fvs = vrfuns map { vrfun => vrfun._3 }
      // All val_p ts ..
      val ts = wrap(((funNames zip wrapNames).toSeq zip fvs), mainBody) {
        case (((fun, wrapfun), fv), mainBody) => 
					tempLetL(0) { c1 =>
          	tempLetP(CPSBlockSet, Seq(fun, c1, wrapfun)) { t1 =>
            	val fvids = List.tabulate(fv.size)(i => i+1)
            	val funs = List.tabulate(fv.size)(i => fun)
            	wrap(((fv zip fvids).toSeq zip funs), mainBody) {
              	case (((fv, num), fun), mainBody) => 
									tempLetL(num) { c_num =>
                		L.LetP(Symbol.fresh("t"), CPSBlockSet, Seq(fun, c_num, fv), mainBody)
									}
							}
						}
					}
				}

			// val_p f1 = block-alloc-202(|FV|+1)
			// |FV|s
      val fvAbs = fvs map { fv => fv.size + 1 }
      // All val_p fs ..
      val fs = wrap(funNames zip fvAbs, ts) {
        case ( (fn, fvs), ts) => tempLetL(fvs) { c1 =>
          L.LetP(fn, CPSBlockAlloc(202), Seq(c1), ts)
				}
			}

      // Return Def
			var convrfuns = vrfuns map { vrfun => List(vrfun._1,vrfun._2) } 
      L.LetF(convrfuns.flatten, fs)

		// APPF
		// [[fun(retC, args)]] =
    case H.AppF(fun, retC, args) =>
			worker.contains(fun) match {
				// Only Application Function
				case true =>
					// worker_for_fun(retC, args, FVs)
        	val workerFun = worker.apply(fun)
        	val workerName = workerFun._1
        	val fvs = workerFun._2
        	L.AppF(workerName, retC, args ++ fvs)

				// Function with Closure 
				case false =>
					// val_l c1 = 0;
        	tempLetL(0) { c1 =>
						// val_p t1 = block-get(fun, c1);
          	tempLetP(CPSBlockGet, Seq(fun, c1)) { t1 =>
							// t1(retC, fun, args)
            	L.AppF(t1, retC, Seq(fun) ++ args)
						}
					}
			}


    // ********************* Conditionnals ***********************
    // Type tests
    case H.If(MiniScalaBlockP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(0, 0), thenC, elseC)
    // TODO: add missing cases
    case H.If(MiniScalaIntP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1), thenC, elseC)
    case H.If(MiniScalaCharP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1, 1, 0), thenC, elseC)
    case H.If(MiniScalaBoolP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1, 0, 1, 0), thenC, elseC)
    case H.If(MiniScalaUnitP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(0, 0, 1, 0), thenC, elseC)

    // Test primitives (<, >, ==, ...)
    // TODO
		// Logic Less Than <
    case H.If(MiniScalaIntLt, args, thenC, elseC) =>
      L.If(CPSLt, args, thenC, elseC)

		// Logic Greater Than >
    case H.If(MiniScalaIntGt, args, thenC, elseC) =>
      L.If(CPSGt, args, thenC, elseC)

		// Logic Less Than Equal <=
    case H.If(MiniScalaIntLe, args, thenC, elseC) =>
      L.If(CPSLe, args, thenC, elseC)

		// Logic Greater Than Equal >=
    case H.If(MiniScalaIntGe, args, thenC, elseC) =>
      L.If(CPSGe, args, thenC, elseC)

		// Logic Equal ==
    case H.If(MiniScalaEq, args, thenC, elseC) =>
      L.If(CPSEq, args, thenC, elseC)

		// Logic Not Equal !=
    case H.If(MiniScalaNe, args, thenC, elseC) =>
      L.If(CPSNe, args, thenC, elseC)

    // Halt case
    // TODO
    case H.Halt(arg) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(arg, c1)) { t1 =>
          L.Halt(t1)}}
  }

  /*
   * Auxilary function.
   *
   * Example:
   *  // assuming we have a function with symbol f and the return continuation is rc:
   *
   *  val names = Seq("first", "second")
   *  val values = Seq(42, 112)
   *  val inner = L.AppF(f, rc, names)
   *  val res = wrap(names zip values , inner) {
   *    case ((n, v), inner) => L.LetL(n, v, inner)
   *  }
   *
   *  // res is going to be the following L.Tree
   *  L.LetL("first", 42,
   *    L.LetL("second", 112,
   *      L.AppF(f, rc, Seq("first", "second"))
   *    )
   *  )
   */
  private def wrap[T](args: Seq[T], inner: L.Tree)(createLayer: (T, L.Tree) => L.Tree) = {
    def addLayers(args: Seq[T]): L.Tree = args match {
      case h +: t => createLayer(h, addLayers(t))
      case _ => inner
    }
    addLayers(args)
  }

  private def freeVariables(tree: H.Tree)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] = tree match {
    case H.LetL(name, _, body) =>
      freeVariables(body) - name
    case H.LetP(name, _, args, body) =>
      freeVariables(body) - name ++ args
    // TODO: add missing cases
		// Rules on Closure Convention Slide 27
    case H.LetC(cnts, body) =>
      var endSet: Set[Symbol] = Set.empty
			cnts.foreach( cnt =>
				endSet = endSet ++ freeVariables(cnt)
			)
      freeVariables(body) ++ endSet
		case H.LetF(funs, body) =>
			var endSet: Set[Symbol] = Set.empty
			funs.foreach( fun =>
				endSet = endSet ++ freeVariables(fun)
			)
			val funNames = funs map { fun => fun.name }
			freeVariables(body) ++ endSet -- funNames
		case H.AppC(_, args) =>
			args.toSet
		case H.AppF(fun, _, args) =>
			worker.contains(fun) match {
				// Only Application Function
				case true =>
        	args.toSet ++ fun

				// Function with Closure 
				case false =>
					args.toSet ++ Seq(fun)
			}
		case H.If(_, args, _, _) =>
			args.toSet
		case H.Halt(args) =>
			args.toSet
  }

  private def freeVariables(cnt: H.CntDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(cnt.body) -- cnt.args

  private def freeVariables(fun: H.FunDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(fun.body) - fun.name -- fun.args

  // Tree builders

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the given literal value.
   */
  private def tempLetL(v: Int)(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetL(tempSym, v, body(tempSym))
  }

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the result of applying the given
   * primitive to the given arguments.
   */
  private def tempLetP(p: L.ValuePrimitive, args: Seq[L.Name])
                      (body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetP(tempSym, p, args, body(tempSym))
  }

  /**
   * Generate an If tree to check whether the least-significant bits
   * of the value bound to the given name are equal to those passed as
   * argument. The generated If tree will apply continuation tC if it
   * is the case, and eC otherwise. The bits should be ordered with
   * the most-significant one first (e.g. the list (1,1,0) represents
   * the decimal value 6).
   */
  private def ifEqLSB(arg: L.Name, bits: Seq[Int], tC: L.Name, eC: L.Name)
      : L.Tree =
    tempLetL(bitsToIntMSBF(bits map { b => 1 } : _*)) { mask =>
      tempLetP(CPSAnd, Seq(arg, mask)) { masked =>
        tempLetL(bitsToIntMSBF(bits : _*)) { value =>
          L.If(CPSEq, Seq(masked, value), tC, eC) } } }
}
