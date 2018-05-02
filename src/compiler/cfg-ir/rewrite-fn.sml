(* rewrite-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Infrastructure for rewriting the CFG.
 *)

signature REWRITE = sig
    structure IR : SSA
  (* rewrite the right-hand-side of an assignment; returns NONE if there are no changes
   * and SOME assigns if the assignment is rewritten to the assigns list.
   *)
    val doAssign : IR.assign -> (IR.var * IR.rhs) list option
  (* rewrite the rhs of an multi-assignment *)
    val doMAssign : IR.massign -> (IR.var * IR.rhs) list option
  (* eliminates unused variables; returns true if any changes *)
    val elimUnusedVars : IR.cfg -> bool
  end

functor RewriteFn (R : REWRITE) : sig

    val transform : R.IR.program -> R.IR.program

  end = struct

    open R

    fun useCount (IR.V{useCnt, ...}) = !useCnt

  (* simplify assignment and multi-assignment statements *)
    fun simplify nd = let
          fun rewrite (SOME[]) = (IR.CFG.deleteNode nd; true)
            | rewrite (SOME assigns) = let
                val assigns = List.map
                      (fn (y, rhs) => (IR.Var.setBinding(y, IR.VB_RHS rhs); IR.ASSGN(y, rhs)))
                        assigns
                in
                  IR.CFG.replaceNodeWithCFG (nd, IR.CFG.mkBlock assigns);
                  true
                end
            | rewrite NONE = false
          in
            case IR.Node.kind nd
             of IR.ASSIGN{stm=(y, rhs), ...} => if (useCount y = 0)
                  then false (* skip unused assignments *)
                  else rewrite (doAssign (y, rhs))
              | IR.MASSIGN{stm, ...} => rewrite (doMAssign stm)
              | _ => false
            (* end case *)
          end

    fun loopToFixPt f = let
          fun loop anyChanges = if f() then loop true else anyChanges
          in
            loop false
          end

    fun transform prog = let
          val IR.Program{
                  props, consts, inputs, constInit, globals,
                  funcs, globInit, strand, create, start, update
                } = prog
          fun simplifyCFG cfg = let
                val changed = ref false
                fun simplify' nd = if simplify nd then changed := true else ()
                in
                  IR.CFG.apply simplify' cfg;
                  !changed
                end
          fun doCFG cfg = let
                val changes = loopToFixPt (fn () => simplifyCFG cfg)
                val changes = loopToFixPt (fn () => elimUnusedVars cfg) orelse changes
                in
                  changes
                end
          fun doOptionalCFG (NONE, changes) = changes
            | doOptionalCFG (SOME cfg, changes) = doCFG cfg orelse changes
          fun doFunc (IR.Func{body, ...}, changes) = doCFG body orelse changes
          fun doStrand (IR.Strand{stateInit, startM, updateM, stabilizeM, ...}) = let
                val changes = doCFG stateInit
                val changes = doOptionalCFG (startM, changes)
                val changes = doCFG updateM orelse changes
                val changes = doOptionalCFG (stabilizeM, changes)
                in
                  changes
                end
          fun doCreate (Create.Create{code, ...}) = doCFG code
          fun optPass () = let
                val changes = doCFG constInit
                val changes = List.foldl doFunc changes funcs
                val changes = doCFG globInit
                val changes = doStrand strand orelse changes
                val changes = doCreate create orelse changes
                val changes = doOptionalCFG (start, changes)
                val changes = doOptionalCFG (update, changes)
                in
                  changes
                end
        (* run the rewrite passes to a fixed point *)
          val _ = loopToFixPt optPass;
        (* filter out unused globals. Note that we never remove unused inputs, since
         * that would affect the command-line interface.
         *)
          val globals = List.filter (fn x => IR.GlobalVar.useCount x > 0) globals
        (* update the program properties if necessary. *)
          val props = if (null globals) andalso (null inputs)
                then Properties.clearProp Properties.HasGlobals props
                else props
          in
            IR.Program{
                props = props,
                consts = consts,
                inputs = inputs,
                constInit = constInit,
                globals = globals,
                funcs = funcs,
                globInit = globInit,
                create = create,
                strand = strand,
                start = start,
                update = update
              }
          end

  end
