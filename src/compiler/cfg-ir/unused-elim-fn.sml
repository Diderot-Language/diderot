(* unused-elim-fn.sml
 *
 * This module implements a pass over a CFG that eliminates unused variables
 * (i.e., variables with use count = 0).
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

functor UnusedElimFn (
    structure IR : SSA
    val cntUnused : Stats.counter
  ) : sig

  (* reduce the CFG by removing unused variables.  Return true if there were any changes
   * and false if there were no changes.
   *)
    val reduce : IR.cfg -> bool

  end = struct

    fun useCount (IR.V{useCnt, ...}) = !useCnt

  (* adjust a variable's use count *)
    fun decUse (IR.V{useCnt, ...}) = (useCnt := !useCnt - 1)

  (* adjust a global variable's use count *)
    fun decGlobal (IR.GV{useCnt, ...}) = (useCnt := !useCnt - 1)

  (* flag used to mark visited nodes in DFS walk of the CFG *)
    val {getFn, setFn} = PropList.newFlag (fn (IR.ND{props, ...}) => props)

    fun doPhi (y, xs) = if (useCount y = 0)
          then (
            Stats.tick cntUnused;
            List.app (Option.app decUse) xs;
            false)
          else true

    fun reduceNd (nd as IR.ND{kind, ...}) = (case kind
           of IR.JOIN{phis, ...} => (phis := List.filter doPhi (!phis))
            | IR.FOREACH{phis, ...} => (phis := List.filter doPhi (!phis))
            | IR.ASSIGN{stm=(y, rhs), ...} => if (useCount y = 0)
                then (
(*
print(concat["** unused: ", IR.assignToString(y, rhs), "\n"]);
*)
                case rhs
                   of IR.GLOBAL x => (
                        Stats.tick cntUnused; decGlobal x; IR.CFG.deleteNode nd)
                    | IR.STATE _ => (Stats.tick cntUnused; IR.CFG.deleteNode nd)
                    | IR.VAR x => (Stats.tick cntUnused; decUse x; IR.CFG.deleteNode nd)
                    | IR.LIT _ => (Stats.tick cntUnused; IR.CFG.deleteNode nd)
                    | IR.OP(rator, xs) => if IR.Op.isPure rator
                        then (Stats.tick cntUnused; List.app decUse xs; IR.CFG.deleteNode nd)
                        else ()
                    | IR.CONS(xs, _) => (
                        Stats.tick cntUnused; List.app decUse xs; IR.CFG.deleteNode nd)
                    | IR.SEQ(xs, _) => (
                        Stats.tick cntUnused; List.app decUse xs; IR.CFG.deleteNode nd)
                    | IR.EINAPP(_, xs) => (
                        Stats.tick cntUnused; List.app decUse xs; IR.CFG.deleteNode nd)
                    | IR.APPLY(f, xs) => ()
(* FIXME: if we know that "f" is pure, then we can delete it here!
                        Stats.tick cntUnused;
                        IR.Func.decCnt f; List.app decUse xs; IR.CFG.deleteNode nd)
*)
                    | _ => raise Fail("bogus rhs for ASSIGN: " ^ IR.RHS.toString rhs)
                  (* end case *))
                else ()
            | IR.MASSIGN{stm=(ys, rhs), ...} => if List.all (fn y => (useCount y = 0)) ys
                then (case rhs
                   of IR.OP(rator, xs) => if IR.Op.isPure rator
                        then (Stats.tick cntUnused; List.app decUse xs; IR.CFG.deleteNode nd)
                        else ()
(* FIXME: if we know that the body of the mapreduce is pure, then we can delete it here! *)
                    | IR.MAPREDUCE _ => ()
                    | _ => raise Fail("bogus rhs for ASSIGN: " ^ IR.RHS.toString rhs)
                  (* end case *))
                else ()
            | IR.GASSIGN{lhs, rhs, ...} =>
                if IR.GlobalVar.useCount lhs = 0
                  then (Stats.tick cntUnused; decUse rhs; IR.CFG.deleteNode nd)
                  else ()
            | _ => ()
          (* end case *))

  (* we apply the reduction in postorder, since eliminating a variable will
   * decrease the count.
   *)
    fun reduce (IR.CFG{entry, ...}) = let
          val n = Stats.count cntUnused
          fun dfs (nd, l) =
                if getFn nd
                  then l
                  else (
                    setFn (nd, true);
                    List.foldl dfs (nd :: l) (IR.Node.succs nd))
        (* nodes in reverse DFS order *)
          val nodes = dfs (entry, [])
          in
            List.app (fn nd => (reduceNd nd; setFn(nd, false))) nodes;
            Stats.count cntUnused > n
          end

  end
