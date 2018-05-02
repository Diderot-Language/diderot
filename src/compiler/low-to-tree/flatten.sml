(* flatten.sml
 *
 * Perform various transforms on the LowIR in preparation for conversion to TreeIR.  The
 * transformations are:
 *
 *    1) flatten out nested CONS right-hand-sides prior to translation to TreeIR.
 *
 *    2) mark those functions that are used in MAPREDUCE computations
 *
 *    3) replicate shared Range nodes so that they are mapped to for-loops in Tree IR
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure Flatten : sig

    val transform : LowIR.program -> LowIR.program

  (* is a function used in a map-reduce?  Note that we assume that map-reduce functions are
   * _not_ used elsewhere!
   *)
    val isMapFunc : LowIR.func -> bool

  end = struct

    structure IR = LowIR
    structure V = IR.Var
    structure Op = LowOps
    structure Ty = LowTypes
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntFlattenCons          = ST.newCounter "low-flatten:cons"
    val cntSkipCons             = ST.newCounter "low-flatten:skip-cons"
    val cntRepRange             = ST.newCounter "low-flatten:replicate-range"
    val cntUnused               = ST.newCounter "low-flatten:unused"

    structure UnusedElim = UnusedElimFn (
        structure IR = IR
        val cntUnused = cntUnused)

    fun removeUnusedVars cfg = ignore (UnusedElim.reduce cfg)

    val use = LowCensus.use

  (* flag for tracking if a function is used for map-reduce (vs. user-defined functions) *)
    val {setFn = markMapFunc, getFn = isMapFunc} = IR.Func.newFlag ()

  (* get a component of a CONS indexed by the given path.  We return the variable
   * that is bound to the component, plus a list of assignments needed to access the
   * component.  We want to avoid having to lift variables in the global/constant
   * initialization blocks into globals, but we do check to see if we can propogate
   * literals from the global/constant initialization blocks.
   *)
    fun index (x, path) = let
          fun mkIndex (x, path) = let
                val t = IR.Var.new("t", LowTypes.realTy)
                val rator = (case (V.ty x, path)
                       of (Ty.TensorTy[d], [idx]) => Op.VIndex(d, idx)
                        | _ => Op.TensorIndex(V.ty x, path)
                      (* end case *))
                in
                  (t, SOME(IR.ASSGN(t, IR.OP(rator, [use x]))))
                end
          fun getGlobal (gv, path) = let
                fun get (x, []) = (case V.binding x
                       of IR.VB_RHS(rhs as IR.GLOBAL gv) => (LowCensus.incGlob gv; SOME rhs)
                        | IR.VB_RHS(IR.VAR x) => NONE
                        | IR.VB_RHS(lit as IR.LIT _) => SOME lit
                        | _ => NONE
                      (* end case *))
                  | get (x, path as idx::idxs) = (case V.binding x
                       of IR.VB_RHS(IR.GLOBAL gv) => getGlobal (gv, path)
                        | IR.VB_RHS(IR.VAR x) => get (x, path)
                        | IR.VB_RHS(IR.CONS(ys, _)) => get(List.nth(ys, idx), idxs)
                        | _ => NONE
                      (* end case *))
                in
                  if IR.GlobalVar.isInput gv
                    then NONE (* inputs could be different at runtime! *)
                    else (case IR.GlobalVar.bindingOf gv
                       of SOME x => get (x, path)
                        | NONE => NONE
                      (* end case *))
                end
          fun get (x, []) = (x, NONE)
            | get (x, path as idx::idxs) = (case V.binding x
               of IR.VB_RHS rhs => (case rhs
                     of IR.GLOBAL gv => (case getGlobal (gv, path)
                           of SOME rhs => let
                                val t = IR.Var.new("tg", LowTypes.realTy)
                                in
                                  (t, SOME(IR.ASSGN(t, rhs)))
                                end
                            | NONE => mkIndex (x, path)
                          (* end case *))
                      | IR.VAR x => get (x, path)
                      | IR.CONS(ys, _) => get(List.nth(ys, idx), idxs)
                      | _ => mkIndex (x, path)
                    (* end case *))
                | _ => mkIndex (x, path)
              (* end case *))
          in
            get (x, path)
          end
handle ex => (
print(concat["index(", V.toString x, ", [", String.concatWithMap "," Int.toString path, "])\n"]);
raise ex)

  (* flatten CONS(args, TensorTy(n::d::dd)), where n=length args.  We return the
   * flattened list of arguments in order and a list of extra assignments in
   * reverse order.
   *)
    fun flatten (args, d, dd) = let
          fun loop (x, d, [], path, args, stms) = let
                fun getArgs (i, args, stms) =
                      if (i < d)
                        then (case index(x, List.rev(i::path))
                           of (y, NONE) => getArgs (i+1, (use y)::args, stms)
                            | (y, SOME stm) => getArgs (i+1, (use y)::args, stm::stms)
                          (* end case *))
                        else (args, stms)
                in
                  getArgs (0, args, stms)
                end
            | loop (x, d, d'::dd, path, args, stms) = let
                fun lp (i, args, stms) =
                      if (i < d)
                        then let
                          val (args, stms) = loop (x, d', dd, i::path, args, stms)
                          in
                            lp (i+1, args, stms)
                          end
                        else (args, stms)
                in
                  lp (0, args, stms)
                end
          fun flattenArgs ([], args, stms) = (List.rev args, stms)
            | flattenArgs (x::xs, args, stms) = let
                val (args, stms) = loop (x, d, dd, [], args, stms)
                in
                  LowCensus.dec x;
                  flattenArgs (xs, args, stms)
                end
          in
            flattenArgs (args, [], [])
          end (* flatten *)
handle ex => (
print(concat["flatten([", String.concatWithMap "," V.toString args, "], ",
Int.toString d, ", [", String.concatWithMap "," Int.toString dd, "])\n"]);
raise ex)

  (* NOTE:
   * We need to be careful about the order in which nodes get processed, since once
   * a variable's binding is reset, subsequent uses cannot be flattened.  This requirement
   * has implications for both the order in which CFGs are processes and the order in
   * which nodes in a CFG are processed.
   *)

    fun doCFG cfg = let
        (* the nodes in parent-before-child order *)
          val nodes = IR.CFG.sort cfg
        (* filter out the interesting nodes while reversing the order *)
          val nodes = let
                fun f (nd, nds) = (case IR.Node.kind nd
                       of IR.ASSIGN{stm=(_, IR.CONS _), ...} => nd :: nds
                        | IR.MASSIGN{stm=(_, IR.MAPREDUCE _), ...} => nd :: nds
                        | IR.FOREACH _ => nd :: nds
                        | _ => nds
                      (* end case *))
                in
                  List.foldl f [] nodes
                end
        (* process a node; note that the node kinds here should match the filter above *)
          fun doNode nd = (case IR.Node.kind nd
                 of IR.ASSIGN{stm=(_, IR.CONS(_, Ty.TensorTy[_])), ...} => ()
                  | IR.ASSIGN{stm=(y, IR.CONS(xs, ty as Ty.TensorTy(_::d::dd))), ...} =>
                      if (V.useCount y = 0)
                        then ST.tick cntSkipCons (* y will be removed *)
                        else let
                          val (xs', stms) = flatten (xs, d, dd)
                          val cfg' = IR.CFG.mkBlock(List.rev(IR.ASSGN(y, IR.CONS(xs', ty))::stms))
                          in
                            ST.tick cntFlattenCons;
                            IR.CFG.replaceNodeWithCFG (nd, cfg')
                          end
                  | IR.MASSIGN{stm=(_, IR.MAPREDUCE mrs), ...} =>
                      List.app (fn (_, f, _) => markMapFunc(f, true)) mrs
                  | IR.FOREACH{pred=ref predNd, src as ref src', ...} =>
                    (* replicate shared Range nodes so that we can map them to for-loops in Tree IR *)
                      if (V.useCount src' > 1)
                        then (case V.getDef src'
                           of rhs as IR.OP(Op.Range, [lo, hi]) => let
                                val src'' = V.copy src'
                                val nd' = IR.Node.mkASSIGN(src'', rhs)
                                in
                                  ST.tick cntRepRange;
                                  LowCensus.dec src'; LowCensus.inc src'';
                                  LowCensus.inc lo; LowCensus.inc hi;
                                  IR.Node.replaceOutEdge {src = predNd, oldDst=nd, dst=nd'};
                                  IR.Node.setPred (nd', predNd);
                                  IR.Node.addEdge (nd', nd);
                                  src := src''
                                end
                            | _ => ()
                          (* end case *))
                        else ()
                  | _ => raise Fail "bogus node"
                (* end case *))
          in
            List.app doNode nodes;
            removeUnusedVars cfg
          end

    fun transform prog = let
          val IR.Program{constInit, funcs, globInit, strand, create, start, update, ...} = prog
          val IR.Strand{stateInit, startM, updateM, stabilizeM, ...} = strand
          in
            Option.app doCFG update;
            Option.app doCFG start;
            Create.app doCFG create;
            Option.app doCFG stabilizeM;
            doCFG updateM;
            Option.app doCFG startM;
            doCFG stateInit;
            doCFG globInit;
            List.app (fn (IR.Func{body, ...}) => doCFG body) funcs;
            doCFG constInit;
            if Controls.get Ctl.dumpLowIR
              then LowPP.output(Log.logFile(), "after flatten", prog)
              else ();
            prog
          end

  end
