(* census-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Compute use counts for IR variables and initialize their bindings.
 *)

functor CensusFn (IR : SSA) : sig

    structure IR : SSA

    val init : IR.program -> unit

    val inc : IR.var -> unit
    val dec : IR.var -> unit
    val use : IR.var -> IR.var

    val incGlob : IR.global_var -> unit
    val decGlob : IR.global_var -> unit

  end = struct

    structure IR = IR

    fun inc (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)
    fun dec (IR.V{useCnt, ...}) = (useCnt := !useCnt - 1)
    fun use (x as IR.V{useCnt, ...}) = (useCnt := !useCnt + 1; x)

    fun incGlob (IR.GV{useCnt, ...}) = (useCnt := !useCnt + 1)
    fun decGlob (IR.GV{useCnt, ...}) = (useCnt := !useCnt - 1)

    fun init prog = let
          val IR.Program{
                  consts, inputs, globals, constInit, funcs,
                  globInit, strand, create, start, update, ...
                } = prog
          fun clearInput (Inputs.INP{var=IR.GV{useCnt, ...}, ...}) = useCnt := 1
          fun clearGlobal (IR.GV{useCnt, ...}) = useCnt := 0
          fun clearFunc (IR.FV{useCnt, ...}) = useCnt := 0
          fun clearVar (IR.V{useCnt, ...}) = useCnt := 0
        (* clear the counts of the variables defined in a node *)
          fun clearNode (IR.ND{kind, ...}) = (case kind
                 of IR.JOIN{phis, ...} => List.app (fn (x, _) => clearVar x) (!phis)
                  | IR.FOREACH{var, phis, ...} => (
                      clearVar var;
                      List.app (fn (x, _) => clearVar x) (!phis))
                  | IR.ASSIGN{stm=(x, _), ...} => clearVar x
                  | IR.MASSIGN{stm=(xs, _), ...} => List.app clearVar xs
                  | _ => ()
                (* end case *))
          val clearCFG = IR.CFG.apply clearNode
        (* clear the counts of the variables defined in a function definition *)
          fun clearFuncDef (IR.Func{name, params, body}) = (
                clearFunc name; List.app clearVar params;
                clearCFG body)
        (* clear the counts of the variables defined in an strand *)
          fun clearStrand (IR.Strand{params, state, stateInit, startM, updateM, stabilizeM, ...}) = (
                List.app clearVar params;
                clearCFG stateInit;
                Option.app clearCFG startM;
                clearCFG updateM;
                Option.app clearCFG stabilizeM)
        (* increment the use counts of a list of variables *)
          val incList = List.app inc
        (* increment the RHS variables of a phi *)
          fun incPhi (_, ys) = List.app (Option.app inc) ys
        (* increment the counts of the variables used in a node.  The exitCount function
         * is used to count the live variables at exits, since the context affects the
         * treatment of these.
         *)
          fun incNode (IR.ND{kind, ...}) = (case kind
                 of IR.JOIN{phis, ...} => List.app incPhi (!phis)
                  | IR.COND{cond, ...} => inc (!cond)
                  | IR.FOREACH{src, phis, ...} => (
                      inc (!src);
                      List.app incPhi (!phis))
                  | IR.ASSIGN{stm = (y, IR.GLOBAL gv), ...} => incGlob gv
                  | IR.ASSIGN{stm = (y, IR.APPLY(f, xs)), ...} => (IR.Func.use f; incList xs)
                  | IR.ASSIGN{stm = (y, rhs), ...} => IR.RHS.app inc rhs
                  | IR.MASSIGN{stm = (ys, IR.MAPREDUCE mrs), ...} =>
                      List.app (fn (_, f, xs) => (IR.Func.use f; incList xs)) mrs
                  | IR.MASSIGN{stm = (ys, rhs), ...} => IR.RHS.app inc rhs
                  | IR.GASSIGN{rhs, ...} => inc rhs
                  | IR.NEW{args, ...} => incList args
                  | IR.SAVE{rhs, ...} => inc rhs
                  | IR.EXIT{kind=ExitKind.RETURN(SOME x), ...} => inc x
                  | _ => ()
                (* end case *))
          val incCFG = IR.CFG.apply incNode
        (* increment the counts of the variables used in a strand *)
          fun incStrand (IR.Strand{params, state, stateInit, startM, updateM, stabilizeM, ...}) = (
                incCFG stateInit;
                Option.app incCFG startM;
                incCFG updateM;
                Option.app incCFG stabilizeM)
          in
          (* first clear the counts of all variables *)
            List.app clearGlobal consts;
            List.app clearInput inputs;
            List.app clearGlobal globals;
            List.app clearFuncDef funcs;
            clearCFG constInit;
            clearCFG globInit;
            Create.app clearCFG create;
            clearStrand strand;
            Option.app clearCFG start;
            Option.app clearCFG update;
          (* then count uses *)
            List.app (fn (IR.Func{body, ...}) => incCFG body) funcs;
            incCFG constInit;
            incCFG globInit;
            Create.app incCFG create;
            incStrand strand;
            Option.app incCFG start;
            Option.app incCFG update
          end

  end
