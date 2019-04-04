package miniscala

import miniscala.{ SymbolicCMScalaTreeModule => S }
import miniscala.{ SymbolicCPSTreeModule => C }

object CMScalaToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree){_ =>
      val z = Symbol.fresh("c0")
      C.LetL(z, IntLit(0), C.Halt(z))
    }(Set.empty)
  }

  private def nonTail(tree: S.Tree)(ctx: Symbol=>C.Tree)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings

    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        nonTail(value)(v =>
            C.LetP(name, MiniScalaId, Seq(v), nonTail(body)(ctx)))

      // TODO: complete the following cases and add the missing ones.

      // Reference of an immutable variable
      case S.Ref(name) if !mut(name) =>
				// [[name]] ctx = 
				// ctx[name]
        ctx(name)

      // Reference of a mutable variable
      case S.Ref(name) => // if mut(name) =>
				// Create Variables
				val z = Symbol.fresh("z")
				val v = Symbol.fresh("v")
      	// [[name]] ctx = 
				// val_l z = 0;
				C.LetL(z, IntLit(0), 
				// val_p v = block-get(name, z);
				C.LetP(v, MiniScalaBlockGet, Seq(name, z),
				// ctx[v]
				ctx(v)))

			// Declaring of a mutable variable
			case S.VarDec(name, _, value, body) =>
				// Create Variables
				val s = Symbol.fresh("s")
				val z = Symbol.fresh("z")
				val d = Symbol.fresh("d")
				// [[var name = value; body]] ctx = 
				// val_l s = 1;
				C.LetL(s, IntLit(1),  
				// val_p name = block-alloc-242(s);
				C.LetP(name, MiniScalaBlockAlloc(242), Seq(s), 
				// val_l z = 0;
				C.LetL(z, IntLit(0), 
				// [[value]] (lm v
				nonTail(value)(v =>
				// (val_p d = block-set(n1, z, v);
				C.LetP(d, MiniScalaBlockSet, Seq(name, z, v),  
				// [[body]] ctx))
				nonTail(body)(ctx)
				// Add to mut[] set
				(mut + name))))))

			// Reassigining of a mutable variable
			case S.VarAssign(name, value) =>
				// Create Variables
				val z = Symbol.fresh("z")
				val d = Symbol.fresh("d")
				// [[name = value]] ctx = 
				// val_l z = 0;
				C.LetL(z, IntLit(0),  
				// [[value]] (lm v
				nonTail(value)(v =>	
				// (val_p d = block-set(name, z, v);
				C.LetP(d, MiniScalaBlockSet, Seq(name, z, v),  
				// ctx[v]))
				ctx(v))))

			// Setting of a literal (maybe put earlier)
			case S.Lit(value) =>
				// Creating Variables
				val n = Symbol.fresh("n")
				// [[value]] ctx = 
				// val_l n = value;
				C.LetL(n, value,
				// ctx[n]
				ctx(n)) 
				
			// Declaring of all functions
			case S.LetRec(funs, body) =>
				// [[def func1(n_1_1: _, ...) = func1.body; def ...; body]] ctx =
				// def_f func1(c, n_1_1, ...) = 
				C.LetF(funs.map(func => {
				// Creating Variables
				val c = Symbol.fresh("c")
				// Extract Arg Names
				val argsmap = func.args map {case S.Arg(n, _, _) => n}
				// Convert to Sequence
				val argsseq = argsmap.toSeq
				// Define
				C.FunDef(func.name, c, argsseq,
				// { [[func.body]]T c};
				tail(func.body, c))
				// def_f ...;
				}),
				// [[body]] ctx
				nonTail(body)(ctx))
			
			// Application of functions
			case S.App(fname, _, args) =>
				// [[fname(a1, a2, ...)]] ctx = 
				// [[fname]] (lm v
				nonTail(fname)(v =>
				// ([[a1]] (lm v1([[a2]] (lm v2 ...))));
				nonTail_*(args.toSeq)(vi => {
				// Creating Variables
				val c = Symbol.fresh("c")
				val r = Symbol.fresh("r")
				// def_c c(r) = { ctx[r] };
				C.LetC(Seq(C.CntDef(c, Seq(r), ctx(r))),  
				// v(c, v1, v2, ...))
				C.AppF(v, c, vi))}))

			// Calculations of prim
			case S.Prim(op, args) =>
				// Match Logical and Non Logical Primitive
				op match {
					// Logical
					case opl: MiniScalaTestPrimitive =>
						// [[p(e1, e2, ...)]] ctx = 
						// [[if (p(e1, e2, ...)) true else false]] ctx
						nonTail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))))(ctx)
					// Non-Logical
					case opnl: MiniScalaValuePrimitive =>
						// Create Variable
						val p = Symbol.fresh("p")
						// [[p(e1, e2, ...)]] ctx = 
						// val_p p = e1 op e2; ctx[p]
						// ...
						nonTail_*(args.toSeq)(ei => C.LetP(p, opnl, ei, ctx(p)))
				}

			// If else statements
			case S.If(cp, tBranch, eBranch) =>
				// Create Variable
				val r = Symbol.fresh("r")
				// [[if (cp) tBranch else eBranch]] ctx = 
				// def_c c(r) = { ctx[r] };
				tempLetC("c", Seq(r), ctx(r))(c => 
				// def_c ct() = { [[tBranch]]T c};
				tempLetC("ct", Seq(), tail(tBranch, c))(ct => 
				// def_c cf() = { [[eBranch]]T c};
				tempLetC("cf", Seq(), tail(eBranch, c))(cf =>
				// [[cp]]C ct cf
				cond(cp, ct, cf))))

			// While loops
			case S.While(cp, lbody, body) =>
				// Create Variables
				val loop = Symbol.fresh("loop")
				val d = Symbol.fresh("d")
				val da = Symbol.fresh("d")
				// [[while (cp) lbody; body]] ctx = 
				// def_c loop(da) = { 
				C.LetC(Seq(C.CntDef(loop, Seq(da), 
				// def_c c() = { [[body]] ctx};
				tempLetC("c", Seq(), nonTail(body)(ctx))(c =>
				// def_c ct() = { [[lbody]]T loop};
				tempLetC("ct", Seq(), tail(lbody, loop))(ct =>
				// [[cp]]C ct c};
				cond(cp, ct, c))))),
				// val_l d = ();
				C.LetL(d, UnitLit,
				// loop()
				C.AppC(loop, Seq(d))))
    }
  }

  private def nonTail_*(trees: Seq[S.Tree])(ctx: Seq[Symbol]=>C.Tree)(implicit mut: Set[Symbol]): C.Tree =
    trees match {
      case Seq() =>
        ctx(Seq())
      case t +: ts =>
        nonTail(t)(tSym => nonTail_*(ts)(tSyms => ctx(tSym +: tSyms)))
    }

  private def tail(tree: S.Tree, c: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        nonTail(value)(v =>
          C.LetP(name, MiniScalaId, Seq(v), tail(body, c)))

      // TODO: add the missing cases.
			
			// Reference of an immutable variable
      case S.Ref(name) if !mut(name) =>
				// [[name]]T c = 
				// c[name]
        C.AppC(c, Seq(name))

      // Reference of a mutable variable
      case S.Ref(name) => // if mut(name) =>
				// Create Variables
				val z = Symbol.fresh("z")
				val v = Symbol.fresh("v")
      	// [[name]]T c = 
				// val_l z = 0;
				C.LetL(z, IntLit(0), 
				// val_p v = block-get(name, z);
				C.LetP(v, MiniScalaBlockGet, Seq(name, z),
				// c[v]
				C.AppC(c, Seq(v))))

			// Declaring of a mutable variable
			case S.VarDec(name, _, value, body) =>
				// Create Variables
				val s = Symbol.fresh("s")
				val z = Symbol.fresh("z")
				val d = Symbol.fresh("d")
				// [[var name = value; body]]T c = 
				// val_l s = 1;
				C.LetL(s, IntLit(1),  
				// val_p name = block-alloc-242(s);
				C.LetP(name, MiniScalaBlockAlloc(242), Seq(s), 
				// val_l z = 0;
				C.LetL(z, IntLit(0), 
				// [[value]] (lm v
				nonTail(value)(v =>
				// (val_p d = block-set(n1, z, v);
				C.LetP(d, MiniScalaBlockSet, Seq(name, z, v),  
				// [[body]]T c))
				tail(body, c)
				// Add to mut[] set
				(mut + name))))))

			// Reassigining of a mutable variable
			case S.VarAssign(name, value) =>
				// Create Variables
				val z = Symbol.fresh("z")
				val d = Symbol.fresh("d")
				// [[name = value]]T c = 
				// val_l z = 0;
				C.LetL(z, IntLit(0),  
				// [[value]] (lm v
				nonTail(value)(v =>	
				// (val_p d = block-set(name, z, v);
				C.LetP(d, MiniScalaBlockSet, Seq(name, z, v),  
				// c[v]))
				C.AppC(c, Seq(v)))))

			// Setting of a literal (maybe put earlier)
			case S.Lit(value) =>
				// Creating Variables
				val n = Symbol.fresh("n")
				// [[value]]T c = 
				// val_l n = value;
				C.LetL(n, value,
				// c[n]
				C.AppC(c, Seq(n))) 
				
			// Declaring of all functions
			case S.LetRec(funs, body) =>
				// [[def func1(n_1_1: _, ...) = func1.body; def ...; body]]T c =
				// def_f func1(c_new, n_1_1, ...) = 
				C.LetF(funs.map(func => {
				// Creating Variables
				val c_new = Symbol.fresh("c")
				// Extract Arg Names
				val argsmap = func.args map {case S.Arg(n, _, _) => n}
				// Convert to Sequence
				val argsseq = argsmap.toSeq
				// Define
				C.FunDef(func.name, c_new, argsseq,
				// { [[func.body]]T c_new};
				tail(func.body, c_new))
				// def_f ...;
				}),
				// [[body]]T c
				tail(body, c))
			
			// Application of functions (optimized by removing continuation)
			case S.App(fname, _, args) =>
				// [[fname(a1, a2, ...)]]T c = 
				// [[fname]] (lm v
				nonTail(fname)(v =>
				// ([[a1]] (lm v1([[a2]] (lm v2 ...))));
				nonTail_*(args.toSeq)(vi => {
				// v(c, v1, v2, ...)
				C.AppF(v, c, vi)}))

			// Calculations of prim
			case S.Prim(op, args) =>
				// Match Logical and Non Logical Primitive
				op match {
					// Logical
					case opl: MiniScalaTestPrimitive =>
						// [[p(e1, e2, ...)]]T c = 
						// [[if (p(e1, e2, ...)) true else false]]T c
						tail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), c)
					// Non-Logical
					case opnl: MiniScalaValuePrimitive =>
						// Create Variable
						val p = Symbol.fresh("p")
						// [[p(e1, e2, ...)]]T c = 
						// val_p p = e1 op e2; c[p]
						// ...
						nonTail_*(args.toSeq)(ei => C.LetP(p, opnl, ei, C.AppC(c, Seq(p))))
				}

			// If else statements (optimized by removing condition)
			case S.If(cp, tBranch, eBranch) =>
				// [[if (cp) tBranch else eBranch]]T c = 
				// def_c ct() = { [[tBranch]]T c};
				tempLetC("ct", Seq(), tail(tBranch, c))(ct => 
				// def_c cf() = { [[eBranch]]T c};
				tempLetC("cf", Seq(), tail(eBranch, c))(cf =>
				// [[cp]]C ct cf
				cond(cp, ct, cf)))

			// While loops
			case S.While(cp, lbody, body) =>
				// Create Variables
				val loop = Symbol.fresh("loop")
				val d = Symbol.fresh("d")
				val da = Symbol.fresh("d")
				// [[while (cp) lbody; body]]T c = 
				// def_c loop(da) = { 
				C.LetC(Seq(C.CntDef(loop, Seq(da), 
				// def_c c() = { [[body]]T c};
				tempLetC("c", Seq(), tail(body, c))(c =>
				// def_c ct() = { [[lbody]]T loop};
				tempLetC("ct", Seq(), tail(lbody, loop))(ct =>
				// [[cp]]C ct c};
				cond(cp, ct, c))))),
				// val_l d = ();
				C.LetL(d, UnitLit,
				// loop()
				C.AppC(loop, Seq(d))))
		
    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol)(implicit mut: Set[Symbol]): C.Tree = {

    def litToCont(l: CMScalaLiteral): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      // TODO add missing cases
			// <Branch> <Bool>
			case S.If(cp, tBranch, S.Lit(tl)) =>
				// Create Variables
				val ac = Symbol.fresh("ac")
				// [[if (cp) tBranch else true|false]]C ct cf =
				// def_c ac() = {				
				val acc = C.CntDef(ac, Seq(),
				// [[tBranch]] ct cf}; 
				cond(tBranch, trueC, falseC))
				// [[cp]]C ac ct|cf	
				C.LetC(Seq(acc), cond(cp, ac, litToCont(tl)))
	
			// <Bool> <Branch>
			case S.If(cp, S.Lit(tl), eBranch) =>
				// Create Variables
				val ac = Symbol.fresh("ac")
				// [[if (cp) true|false else eBranch]]C ct cf =
				// def_c ac() = {				
				val acc = C.CntDef(ac, Seq(),
				// [[eBranch]] ct cf}; 
				cond(eBranch, trueC, falseC))
				// [[cp]]C ct|cf ac	
				C.LetC(Seq(acc), cond(cp, litToCont(tl), ac))

      case S.Prim(p: MiniScalaTestPrimitive, args) =>
        nonTail_*(args)(as => C.If(p, as, trueC, falseC))

      case other =>
        nonTail(other)(o =>
          nonTail(S.Lit(BooleanLit(false)))(n =>
            C.If(MiniScalaNe, Seq(o, n), trueC, falseC)))
    }
  }

  private def tempLetC(cName: String, args: Seq[C.Name], cBody: C.Tree)
                      (body: C.Name=>C.Tree): C.Tree = {
    val cSym = Symbol.fresh(cName)
    C.LetC(Seq(C.CntDef(cSym, args, cBody)), body(cSym))
  }
}
